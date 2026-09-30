// Runs the ECA server inside a Gondolin micro-VM and relays its JSON-RPC over
// stdio.
//
// The channel has to be byte-exact: ECA frames messages as
// `Content-Length: N\r\n\r\n`, which a PTY's line discipline would corrupt by
// echoing input back and translating \n to \r\n. The SDK's exec is raw, so
// this deliberately does not use the `gondolin bash` CLI, which attaches a
// PTY.
//
// Home Manager wraps this, with the configured arguments, as `eca-sandbox'.

const lib = process.env.ECA_GONDOLIN_LIB;

if (!lib) {
  process.stderr.write("eca-gondolin: ECA_GONDOLIN_LIB is unset\n");
  process.exit(2);
}

const {VM, createHttpHooks, RealFSProvider, ReadonlyProvider, buildAssets, verifyAssets} = await import(lib);

const path = await import("node:path");
const fs = await import("node:fs");
const os = await import("node:os");

// Resolved the way ECA resolves them: the XDG variable when it is absolute,
// and the conventional directory under HOME otherwise.
const xdgDir = (name, fallback) => {
  const value = process.env[name];

  return value && path.isAbsolute(value) ? value : path.join(os.homedir(), fallback);
};

const hostConfig = xdgDir("XDG_CONFIG_HOME", ".config");
const hostCache = xdgDir("XDG_CACHE_HOME", ".cache");

// The ECA_SANDBOX_* variables are shared with the bubblewrap backend.
const parseArgs = (argv) => {
  const env = process.env;
  const opts = {
    workspace: env.ECA_SANDBOX_WORKSPACE || process.cwd(),
    guestPath: env.ECA_SANDBOX_GUEST_PATH || null,
    eca: env.ECA_SANDBOX_ECA || null,
    image: env.ECA_SANDBOX_IMAGE || null,
    log: env.ECA_SANDBOX_LOG || null,
    config: env.ECA_SANDBOX_CONFIG || path.join(hostConfig, "eca"),
    state: env.ECA_SANDBOX_STATE || path.join(hostCache, "eca-gondolin"),
    // No default: the allowlist has to be able to express "nothing", which it
    // cannot if some host is always in it. ECA starts without reaching the
    // model catalogue, so there is nothing that must be reachable.
    allowedHosts: (env.ECA_SANDBOX_ALLOW_HOSTS || "")
      .split(",")
      .map((h) => h.trim())
      .filter(Boolean),
    // Everything is reachable and everything is recorded. The proxy still
    // terminates TLS, so `--log' sees each request either way: the choice is
    // whether the boundary refuses traffic or only watches it.
    observe: env.ECA_SANDBOX_OBSERVE === "1",
    shareLogin: env.ECA_SANDBOX_SHARE_LOGIN === "1",
    // Refused whichever mode is in force, so a host can be shut out of an
    // otherwise open session without naming every host that stays open.
    deniedHosts: [],
    tcpMaps: [],
    httpMaps: [],
    env: {},
    command: [],
  };

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    const value = () => {
      if (i + 1 >= argv.length) {
        process.stderr.write(`eca-gondolin: ${arg} needs a value\n`);
        process.exit(2);
      }

      return argv[++i];
    };

    if (arg === "--") {
      opts.command = argv.slice(i + 1);
      break;
    } else if (arg === "--workspace") {
      opts.workspace = value();
    } else if (arg === "--guest-path") {
      opts.guestPath = value();
    } else if (arg === "--eca") {
      opts.eca = value();
    } else if (arg === "--image") {
      opts.image = value();
    } else if (arg === "--config") {
      opts.config = value();
    } else if (arg === "--state") {
      opts.state = value();
    } else if (arg === "--log") {
      opts.log = value();
    } else if (arg === "--allow-host") {
      opts.allowedHosts.push(value());
    } else if (arg === "--observe") {
      opts.observe = true;
    } else if (arg === "--share-login") {
      opts.shareLogin = true;
    } else if (arg === "--deny-host") {
      opts.deniedHosts.push(value());
    } else if (arg === "--tcp-map") {
      opts.tcpMaps.push(value());
    } else if (arg === "--http-map") {
      opts.httpMaps.push(value());
    } else if (arg === "--env") {
      const spec = value();
      const eq = spec.indexOf("=");

      if (eq <= 0) {
        process.stderr.write(`eca-gondolin: --env expects KEY=VALUE, got ${spec}\n`);
        process.exit(2);
      }

      opts.env[spec.slice(0, eq)] = spec.slice(eq + 1);
    } else {
      process.stderr.write(`eca-gondolin: unknown option ${arg}\n`);
      process.exit(2);
    }
  }

  return opts;
};

const opts = parseArgs(process.argv.slice(2));

if (!opts.image) {
  const {ensureImage, managedImageOptions} = await import("./image.mjs");
  opts.image = await ensureImage({...managedImageOptions(), buildAssets, verifyAssets});
}

// Every tool the agent runs is a child of the server process, so the VM
// boundary covers the whole tool surface rather than just shell commands.
const mounts = {};

const workspace = path.resolve(opts.workspace);

// Mounting the workspace where it already lives means the paths the server
// reports are the paths the editor already has, so no prefix map is needed in
// either direction. Over TRAMP that is the difference between working and
// not: the outbound translation strips the TRAMP prefix before applying any
// map, while the inbound one never puts it back, so an explicit mapping
// cannot satisfy both directions at once.
const guestPath = opts.guestPath ?? workspace;

mounts[guestPath] = new RealFSProvider(workspace);

// The guest image has no copy of the server, so a Linux build is mounted
// read-only rather than baked in.
if (opts.eca) {
  mounts["/opt/eca"] = new ReadonlyProvider(
    new RealFSProvider(path.resolve(path.dirname(opts.eca))),
  );
}

// Skills are read from ~/.config/eca/skills, and logins and chat history are
// written under ~/.cache/eca. Without both mounted the guest starts from
// nothing every session: no skills, and a login that has to be redone.
const GUEST_HOME = "/root";

// Home-manager writes this tree as symlinks into the Nix store, which the
// guest has no copy of. Mounted as it stands, every leaf dangles there: the
// skill and agent directories list normally and not one of their files can be
// opened. Copying with the links resolved is what puts the contents in the
// guest, and it is small enough to do on each start.
// `cp' with `dereference' resolves only what it is handed, not the links it
// finds below that, so the copy has to walk the tree itself: `stat' follows a
// link and `copyFile' reads through one, which together turn each entry into
// a file the guest can open.
// `include' filters only the entries directly under `from'.
const copyResolved = (from, to, include = () => true) => {
  fs.mkdirSync(to, {recursive: true});

  for (const entry of fs.readdirSync(from, {withFileTypes: true})) {
    if (!include(entry.name)) continue;

    const source = path.join(from, entry.name);
    const target = path.join(to, entry.name);

    let stat;

    try {
      stat = fs.statSync(source);
    } catch {
      // A link whose target is gone is the one thing worth stepping over
      // rather than failing the session for.
      continue;
    }

    if (stat.isDirectory()) {
      copyResolved(source, target);
    } else {
      fs.copyFileSync(source, target);
    }
  }
};

// Most skills drive an editor, a notifier or a command installed on the host,
// and none of that exists in the guest. Carried across regardless they are
// worse than absent: the agent is told they exist and spends a turn calling
// one. The config says which may come, and a config that does not say is
// taken at its word rather than second-guessed.
const sandboxSkills = (config) => {
  try {
    const listed = JSON.parse(
      fs.readFileSync(path.join(config, "sandbox-skills.json"), "utf8"),
    );

    if (Array.isArray(listed)) return new Set(listed);
  } catch {
    // Absent or unreadable: nothing is claimed, so nothing is withheld.
  }

  return null;
};

let staged = null;

if (opts.config) {
  const config = path.resolve(opts.config);

  if (fs.existsSync(config)) {
    staged = fs.mkdtempSync(path.join(os.tmpdir(), "eca-gondolin-config-"));

    // Covers a failed start and every later exit short of SIGKILL.
    process.on("exit", () => fs.rmSync(staged, {recursive: true, force: true}));

    const keep = sandboxSkills(config);

    copyResolved(config, staged, (name) => !keep || name !== "skills");

    const skills = path.join(config, "skills");

    if (keep && fs.existsSync(skills)) {
      copyResolved(skills, path.join(staged, "skills"), (name) => keep.has(name));
    }

    mounts[`${GUEST_HOME}/.config/eca`] = new ReadonlyProvider(
      new RealFSProvider(staged),
    );
  }
}

if (opts.state) {
  const state = path.resolve(opts.state);

  fs.mkdirSync(state, {recursive: true});

  // `/login' opens a browser and answers on a loopback port, neither of which
  // the guest has, so a hosted provider can only be authenticated on this
  // machine. The tokens live in the same file as the chat history, and the
  // guest keeps its own state directory, so they do not arrive on their own.
  if (opts.shareLogin) {
    const file = "db.transit.json";
    const from = path.join(hostCache, "eca", file);
    const to = path.join(state, file);

    if (!fs.existsSync(from)) {
      process.stderr.write(
        `eca-gondolin: --share-login found no ${from}; log in on this machine first\n`,
      );
    } else if (fs.existsSync(to)) {
      // Left alone once it is there: the guest refreshes these tokens as it
      // runs, and providers commonly retire the old one when it does, so
      // overwriting its copy each start would discard the live credential.
      // Delete the file to take this machine's again.
      process.stderr.write(
        "eca-gondolin: --share-login kept the guest's existing credentials\n",
      );
    } else {
      fs.copyFileSync(from, to);
      fs.chmodSync(to, 0o600);

      process.stderr.write(
        "eca-gondolin: --share-login copied provider tokens into the guest\n",
      );
    }
  }

  mounts[`${GUEST_HOME}/.cache/eca`] = new RealFSProvider(state);
}

const command = opts.command.length
  ? opts.command
  : [opts.eca ? `/opt/eca/${path.basename(opts.eca)}` : "eca", "server"];

const appendLog = (record) => {
  if (!opts.log) return;

  try {
    fs.appendFileSync(opts.log, JSON.stringify(record) + "\n");
  } catch {
    // Observability is best-effort: a failed write must not take the
    // session down.
  }
};

// Both map options read GUEST_HOST[:PORT]=UPSTREAM_HOST:PORT. A guest name
// has to be synthetic: `localhost' resolves inside the VM and never reaches
// the resolver that would map it back to a service on this machine.
const parseMap = (spec) => {
  const eq = spec.indexOf("=");

  if (eq <= 0 || eq === spec.length - 1) {
    process.stderr.write(
      `eca-gondolin: expected GUEST_HOST[:PORT]=UPSTREAM_HOST:PORT, got ${spec}\n`,
    );
    process.exit(2);
  }

  return [spec.slice(0, eq).trim(), spec.slice(eq + 1).trim()];
};

const splitHostPort = (value, what) => {
  const colon = value.lastIndexOf(":");
  const port = colon < 0 ? NaN : Number(value.slice(colon + 1));

  if (!Number.isInteger(port) || port <= 0) {
    process.stderr.write(`eca-gondolin: ${what} needs a port, got ${value}\n`);
    process.exit(2);
  }

  return {host: value.slice(0, colon), port};
};

// Forwarded as raw TCP, below the proxy: the hooks never see this traffic, so
// a tcp map is reachability without observability. Prefer `--http-map' for
// anything speaking HTTP.
const tcpHosts = Object.fromEntries(opts.tcpMaps.map(parseMap));
const mapped = Object.keys(tcpHosts).length > 0;

// An http map instead rewrites the request as it passes through the proxy, so
// the traffic stays logged and policed. It also repoints the `Host' header,
// which a raw forward leaves naming the guest-side name -- enough on its own
// for a server bound to loopback to refuse the request as DNS rebinding.
const httpMaps = opts.httpMaps.map(parseMap).map(([guest, upstream]) => {
  const colon = guest.lastIndexOf(":");
  const guestPort = colon < 0 ? null : Number(guest.slice(colon + 1));

  return {
    host: colon < 0 ? guest : guest.slice(0, colon),
    port: Number.isInteger(guestPort) && guestPort > 0 ? guestPort : null,
    upstream: splitHostPort(upstream, "http map upstream"),
  };
});

// The upstream name is what every policy check sees, because the rewrite runs
// before them: `onRequest' is not marked early-policy-safe, so Gondolin skips
// its pre-body precheck and evaluates the rewritten request instead.
const upstreamHosts = new Set(httpMaps.map((m) => m.upstream.host));

// Loopback and private ranges are refused by default; naming the upstream
// here is what makes a service on this machine reachable at all.
const allowedInternalHosts = [...upstreamHosts];

for (const host of upstreamHosts) {
  if (!opts.allowedHosts.includes(host)) {
    opts.allowedHosts.push(host);
  }
}

// Allowing the upstream host would otherwise expose every port it listens on,
// so the mapped ports are the only ones that may be dialled.
const upstreamTargets = new Set(
  httpMaps.map((m) => `${m.upstream.host}:${m.upstream.port}`),
);

const rewrite = (req) => {
  let url;

  try {
    url = new URL(req.url);
  } catch {
    return undefined;
  }

  const port = Number(url.port) || (url.protocol === "https:" ? 443 : 80);
  const map = httpMaps.find(
    (m) => m.host === url.hostname && (m.port === null || m.port === port),
  );

  if (!map) return undefined;

  url.hostname = map.upstream.host;
  url.port = String(map.upstream.port);

  // Dropping it lets the client derive the header from the rewritten URL; a
  // copied one would still name the guest-side host.
  const headers = new Headers(req.headers);

  headers.delete("host");

  const hasBody = req.method !== "GET" && req.method !== "HEAD";

  return new Request(url.toString(), {
    method: req.method,
    headers,
    ...(hasBody ? {body: req.body, duplex: "half"} : {}),
  });
};

// Gondolin terminates TLS with a CA it mints and installs in the guest trust
// store, so these see decrypted requests without the server being configured
// to trust anything.
// Returns the hooks alongside the environment the guest needs for secret
// placeholders, so both have to be destructured rather than passed through
// whole.
// `*' alone matches everything; a leading `*.' matches any subdomain and the
// domain itself. Anything else is the host exactly.
const matchesHost = (hostname, pattern) => {
  if (pattern === "*") return true;

  if (pattern.startsWith("*.")) {
    const domain = pattern.slice(2);

    return hostname === domain || hostname.endsWith(`.${domain}`);
  }

  return hostname === pattern;
};

const denied = (hostname) =>
  opts.deniedHosts.some((pattern) => matchesHost(hostname, pattern));

if (opts.observe && opts.allowedHosts.length > 0) {
  process.stderr.write(
    "eca-gondolin: --observe reaches every host, so --allow-host adds nothing\n",
  );
}

if (opts.observe) {
  process.stderr.write(
    "eca-gondolin: observing egress, not restricting it" +
      (opts.deniedHosts.length > 0
        ? ` (except ${opts.deniedHosts.join(", ")})`
        : "") +
      (opts.log ? "" : "; no --log, so nothing is being recorded either") +
      "\n",
  );
}

const {httpHooks, env} = createHttpHooks({
  // Undefined is how the allowlist is turned off altogether; an array, even
  // an empty one, means the hosts in it and nothing else.
  allowedHosts: opts.observe ? undefined : opts.allowedHosts,
  allowedInternalHosts,

  // Reached only once the allowlist has already admitted the host, so this
  // can narrow that decision but never widen it.
  isIpAllowed: (info) =>
    !denied(info.hostname) &&
    (!upstreamHosts.has(info.hostname) ||
      upstreamTargets.has(`${info.hostname}:${info.port}`)),

  onRequest: (req) => {
    const next = rewrite(req);

    appendLog({
      at: new Date().toISOString(),
      dir: "request",
      method: req?.method,
      url: req?.url,
      ...(next ? {to: next.url} : {}),
    });

    return next;
  },

  // The response itself carries no URL; it arrives with the request it
  // answers, and a request logged without a matching response was refused.
  onResponse: (res, req) => {
    appendLog({
      at: new Date().toISOString(),
      dir: "response",
      status: res?.status,
      url: req?.url,
    });
  },
});

const vm = new VM({
  vfs: {mounts},
  httpHooks,

  // The guest image points the XDG variables at /tmp, and the server resolves
  // its config and cache through those rather than through HOME, so setting
  // HOME alone leaves both mounts unused: skills stay invisible and logins are
  // discarded with the VM.
  env: {
    ...(env ?? {}),
    HOME: GUEST_HOME,
    XDG_CONFIG_HOME: `${GUEST_HOME}/.config`,
    XDG_CACHE_HOME: `${GUEST_HOME}/.cache`,
    ...opts.env,
  },

  // Anything not resolvable is unreachable, which is what makes the
  // allowlist an enforced boundary rather than a cooperative one.
  dns: {
    mode: "synthetic",
    ...(mapped ? {syntheticHostMapping: "per-host"} : {}),
  },

  ...(mapped ? {tcp: {hosts: tcpHosts}} : {}),

  // The stock guest is Alpine, whose musl has no glibc loader, so the
  // server's native build cannot start there. An image built with `gcompat'
  // supplies /lib/ld-linux-aarch64.so.1.
  ...(opts.image ? {sandbox: {imagePath: opts.image}} : {}),
});

let closed = false;

const shutdown = async (code) => {
  if (closed) return;
  closed = true;

  try {
    await vm.close();
  } catch {
    // Already gone.
  }

  process.exit(code);
};

for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"]) {
  process.on(signal, () => void shutdown(0));
}

await vm.start();

// A single string runs through the guest's login shell, which is what makes
// the executable resolvable from $PATH. The array form skips the shell and so
// skips $PATH too, and passing `argv' alongside a string would hand the
// entries to the shell as positional parameters rather than as arguments.
const quote = (s) => `'${String(s).replaceAll("'", `'\\''`)}'`;

const proc = vm.exec(command.map(quote).join(" "), {
  cwd: guestPath,
  stdin: process.stdin,
  stdout: "pipe",
  stderr: "pipe",
});

const relay = async (pipe, sink) => {
  if (!pipe) return;

  for await (const chunk of pipe) {
    sink.write(chunk);
  }
};

await Promise.all([
  relay(proc.session.stdoutPipe, process.stdout),
  relay(proc.session.stderrPipe, process.stderr),
]);

const result = await proc.result;

await shutdown(result?.exitCode ?? 0);
