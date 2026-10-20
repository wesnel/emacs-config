import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {spawnSync} from "node:child_process";
import {fileURLToPath} from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const runner = path.join(here, "../overlays/offline-run/offline-run.mjs");
const stub = path.join(here, "fixtures/gondolin-stub.mjs");

const fixture = (t) => {
  const root = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), "offline-run-test-")));
  t.after(() => fs.rmSync(root, {recursive: true, force: true}));
  const dirs = Object.fromEntries(
    ["workspace", "tmp", "home"].map((name) => {
      const dir = path.join(root, name);
      fs.mkdirSync(dir);
      return [name, dir];
    }),
  );
  const report = path.join(root, "report.json");

  const run = (args = ["--", "true"], env = {}) => {
    fs.rmSync(report, {force: true});
    const result = spawnSync(process.execPath, [runner, "--image", "/stub-image", ...args], {
      cwd: dirs.workspace,
      encoding: "utf8",
      env: {
        PATH: process.env.PATH,
        HOME: dirs.home,
        TMPDIR: dirs.tmp,
        OFFLINE_RUN_GONDOLIN_LIB: stub,
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
  for (const flag of ["--workspace", "--guest-path", "--ro-mount", "--memory", "--cpus", "--state", "--config"]) {
    const result = run([flag]);
    assert.equal(result.status, 2, flag);
    assert.match(result.stderr, new RegExp(`${flag} needs a value`));
  }
});

test("an unknown option and a missing command are refused", (t) => {
  const {run} = fixture(t);
  for (const args of [["--allow-host", "example.com", "--", "true"], ["--env", "A=b", "--", "true"]]) {
    const result = run(args);
    assert.equal(result.status, 2, args[0]);
    assert.match(result.stderr, new RegExp(`unknown option ${args[0]}`));
    assert.equal(result.report, null);
  }
  const bare = run([]);
  assert.equal(bare.status, 2);
  assert.match(bare.stderr, /runs only a command given after --/);
});

test("the VM has no network and the workspace read-only", (t) => {
  const {dirs, run} = fixture(t);
  const result = run(["--", "go", "test", "./..."]);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.sandbox, {imagePath: "/stub-image", netEnabled: false});
  assert.equal(result.report.hooksCreated, false);
  assert.equal(result.report.httpHooks, false);
  assert.equal(result.report.dns, null);
  assert.equal(result.report.tcp, null);
  assert.deepEqual(Object.keys(result.report.mounts), [dirs.workspace]);
  assert.deepEqual(result.report.mounts[dirs.workspace], {root: dirs.workspace, readonly: true});
  assert.equal(result.report.cwd, dirs.workspace);
  assert.equal(result.report.command, "'go' 'test' './...'");
  assert.equal(result.report.env.HOME, "/root");
  assert.equal(result.report.memory, null);
  assert.equal(result.report.cpus, null);
});

test("--offline and an empty --state or --config are accepted and change nothing", (t) => {
  const {dirs, run} = fixture(t);
  const plain = run();
  const given = run(["--offline", "--state", "", "--config", "", "--", "true"]);
  assert.equal(given.status, 0, given.stderr);
  assert.deepEqual(given.report, plain.report);
  for (const flag of ["--state", "--config"]) {
    const result = run([flag, dirs.tmp, "--", "true"]);
    assert.equal(result.status, 2, flag);
    assert.match(result.stderr, new RegExp(`${flag} takes only an empty value`));
    assert.equal(result.report, null);
  }
});

test("--workspace and --guest-path choose what is mounted and where it runs", (t) => {
  const {root, dirs, run} = fixture(t);
  const other = path.join(root, "other");
  fs.mkdirSync(other);
  const result = run(["--workspace", other, "--guest-path", "/src/", "--", "true"]);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.mounts["/src"], {root: other, readonly: true});
  assert.equal(result.report.mounts[dirs.workspace], undefined);
  assert.equal(result.report.cwd, "/src");
  for (const guest of ["src", "/", "//"]) {
    const refused = run(["--guest-path", guest, "--", "true"]);
    assert.equal(refused.status, 2, guest);
    assert.match(refused.stderr, /--guest-path expects an absolute path other than \//, guest);
  }
});

test("--ro-mount mounts a host directory read-only", (t) => {
  const {root, run} = fixture(t);
  const [a, b] = ["a", "b"].map((name) => {
    const dir = path.join(root, name);
    fs.mkdirSync(dir);
    return dir;
  });
  const result = run(["--ro-mount", `${a}:/opt/a`, "--ro-mount", `${b}:/opt/b`, "--", "true"]);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(result.report.mounts["/opt/a"], {root: a, readonly: true});
  assert.deepEqual(result.report.mounts["/opt/b"], {root: b, readonly: true});
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
    [["--ro-mount", `${dir}:/`], /--ro-mount expects HOST:GUEST/],
    [["--ro-mount", `${path.join(root, "absent")}:/opt/a`], /--ro-mount .*absent is not a directory/],
    [["--ro-mount", `${file}:/opt/a`], /--ro-mount .*file is not a directory/],
    [["--ro-mount", `${dir}:/opt/a`, "--ro-mount", `${dir}:/opt/a/`], /--ro-mount \/opt\/a is already mounted/],
    [["--ro-mount", `${dir}:${dirs.workspace}`], /is already mounted/],
  ];
  for (const [args, message] of cases) {
    const result = run([...args, "--", "true"]);
    assert.equal(result.status, 2, args.join(" "));
    assert.match(result.stderr, message, args.join(" "));
    assert.equal(result.report, null);
  }
});

test("--memory and --cpus size the VM", (t) => {
  const {run} = fixture(t);
  for (const args of [["--memory", "lots"], ["--memory", "4X"], ["--cpus", "0"], ["--cpus", "1.5"], ["--cpus", "x"]]) {
    const refused = run([...args, "--", "true"]);
    assert.equal(refused.status, 2, args.join(" "));
    assert.equal(refused.report, null);
  }
  const result = run(["--memory", "4G", "--cpus", "4", "--", "true"]);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.report.memory, "4G");
  assert.equal(result.report.cpus, 4);
});

test("a VM that does not start fails the run", (t) => {
  const {run} = fixture(t);
  const result = run(undefined, {STUB_FAIL_START: "1"});
  assert.notEqual(result.status, 0);
  assert.equal(result.report, null);
});
