import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {spawnSync} from "node:child_process";
import {fileURLToPath} from "node:url";

const script = process.env.INSTALL_EMACS_SCRIPT ??
  fileURLToPath(new URL("../.github/actions/install-emacs/install.sh", import.meta.url));

// sudo and timeout just run their command, and apt-get is a stand-in: the
// real one needs root and the network. It logs each call and fails the
// first UPDATE_FAILS updates and DOWNLOAD_FAILS downloads, as a stalled
// mirror's `timeout` kill does (exit 124).
const stubs = {
  sudo: `#!/bin/bash\nexec "$@"\n`,
  // Hands its duration to the command it runs, so apt-get can record it.
  timeout: `#!/bin/bash\nexport UNDER_TIMEOUT="$1"\nshift\nexec "$@"\n`,
  "apt-get": `#!/bin/bash
echo "$*" >> "$LOG"
kind=install
case "$*" in
  *" update"*) kind=update ;;
  *--download-only*) kind=download ;;
esac
echo "$kind \${UNDER_TIMEOUT:-none}" >> "$LOG.timeouts"
case "$kind" in
  update) [ "$(grep -c ' update$' "$LOG")" -le "\${UPDATE_FAILS:-0}" ] && exit 124 ;;
  download) [ "$(grep -c -- '--download-only' "$LOG")" -le "\${DOWNLOAD_FAILS:-0}" ] && exit 124 ;;
esac
exit 0
`,
};

const run = (t, {extra, updateFails = 0, downloadFails = 0}) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "install-emacs-test-"));
  t.after(() => fs.rmSync(root, {recursive: true, force: true}));
  const bin = path.join(root, "bin");
  fs.mkdirSync(bin);
  for (const [name, body] of Object.entries(stubs)) {
    fs.writeFileSync(path.join(bin, name), body, {mode: 0o755});
  }
  const log = path.join(root, "log");
  fs.writeFileSync(log, "");
  const env = {
    PATH: `${bin}:${process.env.PATH}`,
    LOG: log,
    UPDATE_FAILS: String(updateFails),
    DOWNLOAD_FAILS: String(downloadFails),
  };
  if (extra !== undefined) env.EXTRA_PACKAGES = extra;
  const result = spawnSync("bash", [script], {env, encoding: "utf8"});
  const calls = fs.readFileSync(log, "utf8").trim().split("\n").filter(Boolean);
  const count = (re) => calls.filter((c) => re.test(c)).length;
  // Each call's kind and the `timeout` duration it ran under, or "none".
  const timeouts = fs.readFileSync(`${log}.timeouts`, "utf8").trim().split("\n")
    .filter(Boolean).map((l) => l.split(" "));
  return {
    timeouts,
    status: result.status,
    stderr: result.stderr,
    calls,
    updates: count(/ update$/),
    downloads: count(/--download-only/),
    installs: count(/^install(?!.*--download-only)/),
  };
};

test("a clean run updates, downloads and installs once each", (t) => {
  const r = run(t, {});
  assert.equal(r.status, 0);
  assert.deepEqual([r.updates, r.downloads, r.installs], [1, 1, 1]);
});

test("update and download run under timeout 40, the install under none", (t) => {
  const r = run(t, {});
  assert.deepEqual(r.timeouts, [["update", "40"], ["download", "40"], ["install", "none"]]);
});

test("a download that times out once is retried, then installed", (t) => {
  const r = run(t, {downloadFails: 1});
  assert.equal(r.status, 0);
  assert.deepEqual([r.updates, r.downloads, r.installs], [1, 2, 1]);
});

test("when every download attempt fails the run fails and nothing is installed", (t) => {
  const r = run(t, {downloadFails: 3});
  assert.notEqual(r.status, 0);
  assert.deepEqual([r.downloads, r.installs], [3, 0]);
});

test("an update that fails once is retried", (t) => {
  const r = run(t, {updateFails: 1});
  assert.equal(r.status, 0);
  assert.deepEqual([r.updates, r.downloads, r.installs], [2, 1, 1]);
});

test("an update that keeps failing warns and goes on to the download", (t) => {
  const r = run(t, {updateFails: 3});
  assert.equal(r.status, 0);
  assert.deepEqual([r.updates, r.downloads, r.installs], [3, 1, 1]);
  assert.match(r.stderr, /warning: apt-get update kept failing/);
});

test("update asks apt to report a transient failure", (t) => {
  const r = run(t, {});
  assert.match(r.calls[0], /-o APT::Update::Error-Mode=any update$/);
});

test("extra packages follow emacs-nox, and none adds nothing", (t) => {
  const none = run(t, {extra: ""});
  const unset = run(t, {});
  const one = run(t, {extra: "elpa-markdown-mode"});
  const download = (r) => r.calls.find((c) => c.includes("--download-only"));
  assert.match(download(none), /-y emacs-nox$/);
  assert.match(download(unset), /-y emacs-nox$/);
  assert.match(download(one), /-y emacs-nox elpa-markdown-mode$/);
  assert.match(one.calls.at(-1), /^install --no-install-recommends -y emacs-nox elpa-markdown-mode$/);
});
