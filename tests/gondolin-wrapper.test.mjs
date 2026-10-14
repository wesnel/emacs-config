import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {spawnSync} from "node:child_process";
import {fileURLToPath} from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const wrapper = path.join(here, "../overlays/eca-gondolin/eca-gondolin.mjs");
const stub = path.join(here, "fixtures/gondolin-stub.mjs");

const fixture = (t) => {
  const root = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), "eca-wrapper-test-")));
  t.after(() => fs.rmSync(root, {recursive: true, force: true}));
  const dirs = Object.fromEntries(
    ["workspace", "config", "state", "tmp", "home"].map((name) => {
      const dir = path.join(root, name);
      fs.mkdirSync(dir);
      return [name, dir];
    }),
  );
  const report = path.join(root, "report.json");

  const run = (args = [], env = {}) => {
    const result = spawnSync(process.execPath, [wrapper, "--image", "/stub-image", ...args], {
      cwd: dirs.workspace,
      encoding: "utf8",
      env: {
        PATH: process.env.PATH,
        HOME: dirs.home,
        TMPDIR: dirs.tmp,
        ECA_GONDOLIN_LIB: stub,
        ECA_SANDBOX_CONFIG: dirs.config,
        ECA_SANDBOX_STATE: dirs.state,
        STUB_REPORT: report,
        ...env,
      },
    });
    return {
      ...result,
      report: fs.existsSync(report) ? JSON.parse(fs.readFileSync(report, "utf8")) : null,
    };
  };

  return {root, dirs, run};
};

test("a flag without its value is refused", (t) => {
  const {run} = fixture(t);
  for (const flag of [
    "--allow-host", "--http-map", "--env", "--secret", "--workspace", "--plans", "--guard-config", "--tools",
    "--ro-mount", "--memory", "--cpus",
  ]) {
    const result = run([flag]);
    assert.equal(result.status, 2, flag);
    assert.match(result.stderr, new RegExp(`${flag} needs a value`));
  }
  assert.match(run(["--env", "NOEQUALS"]).stderr, /--env expects KEY=VALUE/);
});

test("the workspace is mounted where it lives and the server runs there", (t) => {
  const {dirs, run} = fixture(t);
  const result = run([], {ECA_SANDBOX_ECA: "/somewhere/bin/eca"});
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.mounts[dirs.workspace], {root: dirs.workspace, readonly: false});
  assert.deepEqual(result.report.mounts["/opt/eca"], {root: "/somewhere/bin", readonly: true});
  assert.deepEqual(result.report.mounts["/root/.cache/eca"], {root: dirs.state, readonly: false});
  assert.equal(result.report.cwd, dirs.workspace);
  assert.equal(result.report.command, "'/opt/eca/eca' 'server'");
});

test("an empty state or config is not mounted at all", (t) => {
  const {dirs, run} = fixture(t);
  fs.writeFileSync(path.join(dirs.config, "config.json"), "{}");
  const result = run(["--state", "", "--config", ""]);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.mounts["/root/.cache/eca"], undefined);
  assert.equal(result.report.mounts["/root/.config/eca"], undefined);
  assert.deepEqual(fs.readdirSync(dirs.home), []);
  assert.deepEqual(fs.readdirSync(dirs.workspace), []);
});

test("only listed skills reach the guest, with links resolved", (t) => {
  const {root, dirs, run} = fixture(t);
  const store = path.join(root, "store");
  fs.mkdirSync(store);
  fs.writeFileSync(path.join(store, "keep.md"), "kept\n");
  fs.writeFileSync(path.join(store, "drop.md"), "dropped\n");
  for (const name of ["keep", "drop"]) {
    fs.mkdirSync(path.join(dirs.config, "skills", name), {recursive: true});
    fs.symlinkSync(path.join(store, `${name}.md`), path.join(dirs.config, "skills", name, "SKILL.md"));
  }
  fs.symlinkSync(path.join(store, "keep.md"), path.join(dirs.config, "config.json"));
  fs.writeFileSync(path.join(dirs.config, "sandbox-skills.json"), JSON.stringify(["keep"]));

  const result = run();
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.mounts["/root/.config/eca"].readonly, true);
  assert.deepEqual(result.report.config, {
    "config.json": "kept\n",
    "sandbox-skills.json": JSON.stringify(["keep"]),
    "skills/keep/SKILL.md": "kept\n",
  });
});

test("the staged config is removed on exit and after a failed start", (t) => {
  const {dirs, run} = fixture(t);
  fs.writeFileSync(path.join(dirs.config, "config.json"), "{}");
  assert.equal(run().status, 0);
  assert.deepEqual(fs.readdirSync(dirs.tmp), []);
  assert.notEqual(run([], {STUB_FAIL_START: "1"}).status, 0);
  assert.deepEqual(fs.readdirSync(dirs.tmp), []);
});

test("an http map reaches only the mapped upstream port", (t) => {
  const {run} = fixture(t);
  const result = run(["--http-map", "ollama:11434=127.0.0.1:11434", "--env", "A=b=c"], {
    STUB_PROBES: JSON.stringify({
      ip: [
        {hostname: "127.0.0.1", port: 11434},
        {hostname: "127.0.0.1", port: 22},
      ],
      urls: ["http://ollama:11434/api/tags", "http://other:11434/"],
    }),
  });
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.allowedHosts, ["127.0.0.1"]);
  assert.deepEqual(result.report.allowedInternalHosts, ["127.0.0.1"]);
  assert.deepEqual(result.report.ipAllowed, [true, false]);
  assert.deepEqual(result.report.rewritten, ["http://127.0.0.1:11434/api/tags", null]);
  assert.equal(result.report.env.A, "b=c");
});

test("observe lifts the allowlist but deny-host still refuses", (t) => {
  const {run} = fixture(t);
  const result = run(["--observe", "--deny-host", "*.example.com"], {
    STUB_PROBES: JSON.stringify({
      ip: [
        {hostname: "api.example.com", port: 443},
        {hostname: "example.org", port: 443},
      ],
    }),
  });
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.allowedHosts, null);
  assert.deepEqual(result.report.ipAllowed, [false, true]);
});

test("the shared environment variables configure the allowlist", (t) => {
  const {run} = fixture(t);
  const result = run([], {ECA_SANDBOX_ALLOW_HOSTS: "a.example, b.example"});
  assert.deepEqual(result.report.allowedHosts, ["a.example", "b.example"]);
});

test("a secret reaches the hooks and only its placeholder reaches the guest", (t) => {
  const {run} = fixture(t);
  const result = run(["--secret", "TOKEN=api.example, *.example.org"], {TOKEN: "s3cret"});
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.secrets, {
    TOKEN: {hosts: ["api.example", "*.example.org"], value: "s3cret"},
  });
  assert.equal(result.report.env.TOKEN, "placeholder-TOKEN");
  assert.ok(!JSON.stringify(result.report.env).includes("s3cret"));
});

test("a secret that cannot be resolved is refused", (t) => {
  const {run} = fixture(t);
  assert.match(run(["--secret", "TOKEN=api.example"]).stderr, /--secret TOKEN is not set/);
  for (const spec of ["TOKEN", "=api.example", "TOKEN="]) {
    const result = run(["--secret", spec], {TOKEN: "s3cret"});
    assert.equal(result.status, 2, spec);
    assert.match(result.stderr, /--secret expects NAME=HOST/);
  }
  const both = run(["--secret", "TOKEN=api.example", "--env", "TOKEN=x"], {TOKEN: "s3cret"});
  assert.equal(both.status, 2);
  assert.match(both.stderr, /TOKEN is given by both --env and --secret/);
});

// Stands in for `sandbox-relay serve': records its arguments and announces a
// port.
const fakeRelay = (dirs, announce = true) => {
  const server = path.join(dirs.tmp, "sandbox-relay");
  const args = path.join(dirs.tmp, "sandbox-relay-args");
  fs.writeFileSync(
    server,
    `#!/bin/sh\nprintf '%s\\n' "$@" > '${args}'\n` +
      (announce ? `echo "sandbox-relay: serving x on http://127.0.0.1:4567"\nexec sleep 30\n` : "exit 1\n"),
    {mode: 0o755},
  );
  return {server, args};
};

test("pr-loop is served from this side and reached from the guest", (t) => {
  const {dirs, run} = fixture(t);
  const {server, args} = fakeRelay(dirs);
  const client = path.join(dirs.tmp, "client");
  fs.mkdirSync(client);
  const result = run(["--pr-loop", client], {
    ECA_SANDBOX_RELAY: server,
    STUB_PROBES: JSON.stringify({urls: ["http://sandbox-relay/"]}),
  });
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(fs.readFileSync(args, "utf8").trim().split("\n"), [
    "serve", "--workspace", dirs.workspace, "--guest-path", dirs.workspace, "--listen", "127.0.0.1:0",
    "--program", "pr-loop", "--program", "agent-plan",
  ]);
  assert.deepEqual(result.report.mounts["/usr/local/bin"], {root: client, readonly: true});
  assert.deepEqual(result.report.rewritten, ["http://127.0.0.1:4567/"]);
  assert.equal(result.report.env.SANDBOX_RELAY_URL, "http://sandbox-relay");
  assert.equal(result.report.env.PR_LOOP_SESSION, "1");
  assert.equal(result.report.env.GIT_CONFIG_KEY_0, "commit.gpgsign");
  assert.equal(result.report.env.GIT_CONFIG_VALUE_0, "false");
});

test("a pr-loop session adds the guard hook to the staged config only", (t) => {
  const {dirs, run} = fixture(t);
  const {server} = fakeRelay(dirs);
  const own = {hooks: {mine: {type: "postToolCall", actions: []}}, chat: {defaultTrust: false}};
  fs.writeFileSync(path.join(dirs.config, "config.json"), JSON.stringify(own));

  const loop = run(["--pr-loop", dirs.tmp], {ECA_SANDBOX_RELAY: server});
  assert.equal(loop.status, 0, loop.stderr);
  const staged = JSON.parse(loop.report.config["config.json"]);
  assert.deepEqual(staged.chat, own.chat);
  assert.deepEqual(staged.hooks.mine, own.hooks.mine);
  assert.equal(staged.hooks["pr-loop-guard"].type, "preToolCall");
  assert.deepEqual(staged.hooks["pr-loop-guard"].actions, [{type: "shell", shell: "pr-loop guard"}]);
  assert.deepEqual(JSON.parse(fs.readFileSync(path.join(dirs.config, "config.json"), "utf8")), own);

  const plain = run([]);
  assert.equal(plain.status, 0, plain.stderr);
  assert.equal(JSON.parse(plain.report.config["config.json"]).hooks["pr-loop-guard"], undefined);
});

test("a config that is not plain JSON stops a pr-loop session", (t) => {
  const {dirs, run} = fixture(t);
  const {server} = fakeRelay(dirs);
  fs.writeFileSync(path.join(dirs.config, "config.json"), "{ // a comment\n}");
  const result = run(["--pr-loop", dirs.tmp], {ECA_SANDBOX_RELAY: server});
  assert.equal(result.status, 2);
  assert.match(result.stderr, /cannot add the pr-loop guard/);
});

test("a pr-loop session without any ECA config still gets the guard", (t) => {
  const {root, dirs, run} = fixture(t);
  const {server} = fakeRelay(dirs);
  const result = run(["--pr-loop", dirs.tmp], {
    ECA_SANDBOX_RELAY: server,
    ECA_SANDBOX_CONFIG: path.join(root, "absent"),
  });
  assert.equal(result.status, 0, result.stderr);
  assert.ok(JSON.parse(result.report.config["config.json"]).hooks["pr-loop-guard"]);
});

test("a relay that does not start stops the wrapper", (t) => {
  const {dirs, run} = fixture(t);
  const {server} = fakeRelay(dirs, false);
  const result = run(["--pr-loop", dirs.tmp], {ECA_SANDBOX_RELAY: server});
  assert.equal(result.status, 2);
  assert.match(result.stderr, /sandbox-relay serve exited before listening/);
  assert.equal(result.report, null);
});

test("the plans directory is writable in the guest where it lives", (t) => {
  const {root, dirs, run} = fixture(t);
  const plans = path.join(root, "notes", "agents");
  fs.mkdirSync(plans, {recursive: true});
  const result = run(["--plans", plans]);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.mounts[plans], {root: plans, readonly: false});
});

test("a pr-loop session logs only through the relay and gets no plans mount", (t) => {
  const {root, dirs, run} = fixture(t);
  const {server} = fakeRelay(dirs);
  const plans = path.join(root, "notes", "agents");
  fs.mkdirSync(plans, {recursive: true});
  const result = run(["--plans", plans, "--pr-loop", dirs.tmp], {ECA_SANDBOX_RELAY: server});
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.mounts[plans], undefined);
});

test("an ordinary session gets the tools on PATH and no relay", (t) => {
  const {dirs, run} = fixture(t);
  const {server, args} = fakeRelay(dirs);
  const tools = path.join(dirs.tmp, "tools");
  fs.mkdirSync(tools);
  const result = run(["--tools", tools], {ECA_SANDBOX_RELAY: server});
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.mounts["/usr/local/bin"], {root: tools, readonly: true});
  assert.equal(fs.existsSync(args), false);
  assert.equal(result.report.env.SANDBOX_RELAY_URL, undefined);
  assert.equal(result.report.env.PR_LOOP_SESSION, undefined);
});

test("a pr-loop session keeps its own client directory over the tools", (t) => {
  const {dirs, run} = fixture(t);
  const {server} = fakeRelay(dirs);
  const client = path.join(dirs.tmp, "client");
  const tools = path.join(dirs.tmp, "tools");
  fs.mkdirSync(client);
  fs.mkdirSync(tools);
  const result = run(["--tools", tools, "--pr-loop", client], {ECA_SANDBOX_RELAY: server});
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.mounts["/usr/local/bin"], {root: client, readonly: true});
  assert.equal(result.report.env.SANDBOX_RELAY_URL, "http://sandbox-relay");
});

test("a tools directory that does not exist is left out with a notice", (t) => {
  const {root, run} = fixture(t);
  const result = run(["--tools", path.join(root, "absent")]);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.mounts["/usr/local/bin"], undefined);
  assert.match(result.stderr, /--tools .*absent.* does not exist/);
});

test("a plans directory that does not exist is left out with a notice", (t) => {
  const {root, run} = fixture(t);
  const plans = path.join(root, "absent");
  const result = run(["--plans", plans]);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.mounts[plans], undefined);
  assert.match(result.stderr, /--plans .*absent.* does not exist/);
});

test("the guard config is readable where agent-plan looks, with links resolved", (t) => {
  const {root, dirs, run} = fixture(t);
  const store = path.join(root, "store.json");
  fs.writeFileSync(store, '{"plans": {"dir": "/x"}}');
  const link = path.join(root, "config.json");
  fs.symlinkSync(store, link);
  const result = run(["--guard-config", link]);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.mounts["/root/.config/agent-tools"].readonly, true);
  assert.deepEqual(result.report.guardConfig, {"config.json": '{"plans": {"dir": "/x"}}'});
  assert.deepEqual(fs.readdirSync(dirs.tmp), []);
});

test("the guard config stays visible to a pr-loop session", (t) => {
  const {root, dirs, run} = fixture(t);
  const {server} = fakeRelay(dirs);
  const file = path.join(root, "guard.json");
  fs.writeFileSync(file, "{}");
  const result = run(["--guard-config", file, "--pr-loop", dirs.tmp], {ECA_SANDBOX_RELAY: server});
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.guardConfig, {"config.json": "{}"});
});

test("a guard config that does not exist is left out with a notice", (t) => {
  const {root, run} = fixture(t);
  const result = run(["--guard-config", path.join(root, "absent.json")]);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.guardConfig, null);
  assert.match(result.stderr, /--guard-config .*absent.json.* does not exist/);
});

test("without --offline the VM keeps its network and default size", (t) => {
  const {run} = fixture(t);
  const result = run();
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.hooksCreated, true);
  assert.equal(result.report.httpHooks, true);
  assert.deepEqual(result.report.dns, {mode: "synthetic"});
  assert.deepEqual(result.report.sandbox, {imagePath: "/stub-image"});
  assert.equal(result.report.memory, null);
  assert.equal(result.report.cpus, null);
});

test("--offline gives the VM no network and the workspace read-only", (t) => {
  const {dirs, run} = fixture(t);
  const result = run(["--offline", "--", "go", "test", "./..."]);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.sandbox, {imagePath: "/stub-image", netEnabled: false});
  assert.equal(result.report.hooksCreated, false);
  assert.equal(result.report.httpHooks, false);
  assert.equal(result.report.dns, null);
  assert.equal(result.report.tcp, null);
  assert.deepEqual(result.report.mounts[dirs.workspace], {root: dirs.workspace, readonly: true});
  assert.equal(result.report.command, "'go' 'test' './...'");
});

test("--offline mounts the workspace at --guest-path and runs there", (t) => {
  const {dirs, run} = fixture(t);
  const result = run(["--offline", "--guest-path", "/src", "--", "true"]);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.mounts["/src"], {root: dirs.workspace, readonly: true});
  assert.equal(result.report.mounts[dirs.workspace], undefined);
  assert.equal(result.report.cwd, "/src");
});

test("--offline refuses an agent's plans, tools and guard config", (t) => {
  const {root, dirs, run} = fixture(t);
  const file = path.join(root, "guard.json");
  fs.writeFileSync(file, "{}");
  for (const [args, flag] of [
    [["--plans", dirs.tmp], "--plans"],
    [["--tools", dirs.tmp], "--tools"],
    [["--guard-config", file], "--guard-config"],
  ]) {
    const result = run(["--offline", ...args, "--", "true"]);
    assert.equal(result.status, 2, flag);
    assert.match(result.stderr, new RegExp(`--offline runs no agent, so it cannot take ${flag}`), flag);
    assert.equal(result.report, null, flag);
  }
});

test("--offline refuses whatever needs the network, and a missing command", (t) => {
  const {root, dirs, run} = fixture(t);
  const cases = [
    [["--allow-host", "example.com"], "--allow-host"],
    [["--observe"], "--observe"],
    [["--deny-host", "example.com"], "--deny-host"],
    [["--http-map", "a:80=127.0.0.1:8080"], "--http-map"],
    [["--tcp-map", "a:22=127.0.0.1:22"], "--tcp-map"],
    [["--secret", "TOKEN=example.com"], "--secret"],
    [["--pr-loop", dirs.tmp], "--pr-loop"],
    [["--share-login"], "--share-login"],
    [["--log", path.join(root, "log.jsonl")], "--log"],
  ];
  for (const [args, flag] of cases) {
    const result = run(["--offline", ...args, "--", "true"], {TOKEN: "s3cret"});
    assert.equal(result.status, 2, flag);
    assert.match(result.stderr, new RegExp(`--offline has no network, so it cannot take ${flag}`), flag);
    assert.equal(result.report, null, flag);
  }
  const env = run(["--offline", "--", "true"], {ECA_SANDBOX_ALLOW_HOSTS: "example.com"});
  assert.equal(env.status, 2);
  assert.match(env.stderr, /cannot take --allow-host/);
  const bare = run(["--offline"]);
  assert.equal(bare.status, 2);
  assert.match(bare.stderr, /--offline runs only a command given after --/);
});

test("--ro-mount mounts a host directory read-only, offline or not", (t) => {
  const {root, run} = fixture(t);
  const [a, b] = ["a", "b"].map((name) => {
    const dir = path.join(root, name);
    fs.mkdirSync(dir);
    return dir;
  });
  for (const extra of [[], ["--offline"]]) {
    const result = run([...extra, "--ro-mount", `${a}:/opt/a`, "--ro-mount", `${b}:/opt/b`, "--", "true"]);
    assert.equal(result.status, 0, result.stderr);
    assert.deepEqual(result.report.mounts["/opt/a"], {root: a, readonly: true});
    assert.deepEqual(result.report.mounts["/opt/b"], {root: b, readonly: true});
  }
});

test("a read-only mount needs a host directory and a guest path of its own", (t) => {
  const {root, dirs, run} = fixture(t);
  const dir = path.join(root, "a");
  const file = path.join(root, "file");
  fs.mkdirSync(dir);
  fs.writeFileSync(file, "");
  const cases = [
    [["--ro-mount", dir], /--ro-mount expects HOST:GUEST/],
    [["--ro-mount", `${dir}:relative`], /--ro-mount expects HOST:GUEST/],
    [["--ro-mount", `${path.join(root, "absent")}:/opt/a`], /--ro-mount .*absent is not a directory/],
    [["--ro-mount", `${file}:/opt/a`], /--ro-mount .*file is not a directory/],
    [["--ro-mount", `${dir}:/opt/a`, "--ro-mount", `${dir}:/opt/a/`], /--ro-mount \/opt\/a is already mounted/],
    [["--ro-mount", `${dir}:${dirs.workspace}`], /is already mounted/],
  ];
  for (const [args, message] of cases) {
    const result = run(args);
    assert.equal(result.status, 2, args.join(" "));
    assert.match(result.stderr, message, args.join(" "));
    assert.equal(result.report, null);
  }
});

test("--memory and --cpus size the VM", (t) => {
  const {run} = fixture(t);
  for (const args of [["--memory", "lots"], ["--memory", "4X"], ["--cpus", "0"], ["--cpus", "1.5"], ["--cpus", "x"]]) {
    const refused = run(args);
    assert.equal(refused.status, 2, args.join(" "));
    assert.equal(refused.report, null);
  }
  const result = run(["--memory", "4G", "--cpus", "4"]);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.memory, "4G");
  assert.equal(result.report.cpus, 4);
});
