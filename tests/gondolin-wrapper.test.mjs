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
  for (const flag of ["--allow-host", "--http-map", "--env", "--workspace"]) {
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
