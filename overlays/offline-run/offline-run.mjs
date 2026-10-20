// Runs one command in a Gondolin micro-VM with no network device, the
// workspace read-only and nothing else of this machine but what --ro-mount
// hands it. For code nobody has vetted for this machine, such as a pull
// request's tests.
//
// Usage: offline-run [--workspace DIR] [--guest-path PATH] [--image IMAGE]
//          [--ro-mount HOST:GUEST]... [--memory SIZE] [--cpus N] -- CMD...

const lib = process.env.OFFLINE_RUN_GONDOLIN_LIB;

if (!lib) {
  process.stderr.write("offline-run: OFFLINE_RUN_GONDOLIN_LIB is unset\n");
  process.exit(2);
}

const {VM, RealFSProvider, ReadonlyProvider, buildAssets, verifyAssets} = await import(lib);

const path = await import("node:path");
const fs = await import("node:fs");

const fail = (message) => {
  process.stderr.write(`offline-run: ${message}\n`);
  process.exit(2);
};

// The options are the same for the bubblewrap runner, offline-run.sh, so that
// one set of arguments is right or wrong for both.
const parseArgs = (argv) => {
  const opts = {
    workspace: process.cwd(),
    guestPath: null,
    image: null,
    roMounts: [],
    memory: null,
    cpus: null,
    command: [],
  };

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    const value = () => {
      if (i + 1 >= argv.length) fail(`${arg} needs a value`);

      return argv[++i];
    };

    if (arg === "--") {
      opts.command = argv.slice(i + 1);
      break;
    } else if (arg === "--offline") {
      // Every run is offline; accepted so that callers may say so.
    } else if (arg === "--state" || arg === "--config") {
      // Accepted empty, meaning nothing mounted, which is all a run gets.
      if (value() !== "") fail(`${arg} takes only an empty value: nothing of this machine is mounted but --ro-mount`);
    } else if (arg === "--workspace") {
      opts.workspace = value();
    } else if (arg === "--guest-path") {
      const guest = value();

      if (!path.posix.isAbsolute(guest) || /^\/+$/.test(guest)) {
        fail(`--guest-path expects an absolute path other than /, got ${guest}`);
      }

      opts.guestPath = path.posix.normalize(guest).replace(/(.)\/+$/, "$1");
    } else if (arg === "--image") {
      opts.image = value();
    } else if (arg === "--ro-mount") {
      // Split at the last colon: the guest side is an absolute path, and
      // host paths are the likelier of the two to contain one.
      const spec = value();
      const colon = spec.lastIndexOf(":");
      const host = spec.slice(0, colon);
      const guest = path.posix.normalize(spec.slice(colon + 1)).replace(/(.)\/+$/, "$1");

      if (colon <= 0 || !path.posix.isAbsolute(guest) || guest === "/") {
        fail(`--ro-mount expects HOST:GUEST with an absolute GUEST, got ${spec}`);
      }

      if (!fs.statSync(host, {throwIfNoEntry: false})?.isDirectory()) {
        fail(`--ro-mount ${host} is not a directory`);
      }

      opts.roMounts.push({host: path.resolve(host), guest});
    } else if (arg === "--memory") {
      // QEMU's size syntax, as Gondolin passes it through.
      opts.memory = value();

      if (!/^[1-9]\d*[KMGT]?$/i.test(opts.memory)) {
        fail(`--memory expects a size such as 4G, got ${opts.memory}`);
      }
    } else if (arg === "--cpus") {
      const spec = value();

      if (!/^[1-9]\d*$/.test(spec)) fail(`--cpus expects a positive whole number, got ${spec}`);

      opts.cpus = Number(spec);
    } else {
      fail(`unknown option ${arg}`);
    }
  }

  if (opts.command.length === 0) fail("runs only a command given after --");

  return opts;
};

const opts = parseArgs(process.argv.slice(2));

const GUEST_HOME = "/root";

const workspace = path.resolve(opts.workspace);
const guestPath = opts.guestPath ?? workspace;

const mounts = {[guestPath]: new ReadonlyProvider(new RealFSProvider(workspace))};

for (const {host, guest} of opts.roMounts) {
  if (guest in mounts) fail(`--ro-mount ${guest} is already mounted`);

  mounts[guest] = new ReadonlyProvider(new RealFSProvider(host));
}

if (!opts.image) {
  const {ensureImage, managedImageOptions} = await import("./image.mjs");
  opts.image = await ensureImage({...managedImageOptions(), buildAssets, verifyAssets});
}

const vm = new VM({
  vfs: {mounts},

  // The image points the XDG variables at /tmp. Under both runners a command
  // instead finds them below its HOME, as tools expect.
  env: {
    HOME: GUEST_HOME,
    XDG_CONFIG_HOME: `${GUEST_HOME}/.config`,
    XDG_CACHE_HOME: `${GUEST_HOME}/.cache`,
  },

  ...(opts.memory ? {memory: opts.memory} : {}),
  ...(opts.cpus ? {cpus: opts.cpus} : {}),
  sandbox: {
    imagePath: opts.image,

    // No network device at all, rather than one behind a proxy that refuses
    // everything: that proxy still looks each name up on this side before
    // refusing it.
    netEnabled: false,
  },
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
//
// The pipes are raw rather than through a PTY, whose line discipline would
// echo input back and rewrite line endings.
const quote = (s) => `'${String(s).replaceAll("'", `'\\''`)}'`;

const proc = vm.exec(opts.command.map(quote).join(" "), {
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
