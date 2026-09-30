import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {pathToFileURL} from "node:url";
import {execFileSync} from "node:child_process";

// Loaded through NODE_OPTIONS by the Nix test, after the installed wrapper
// has set its environment. Exit before the activation entry point downloads
// a full image: exercise the SDK's actual host-tool calls on a small fixture.
const {createInitramfs, createRootfsImage, findMke2fs} = await import(
  new URL("./alpine/utils.js", pathToFileURL(process.env.ECA_GONDOLIN_LIB))
);
const directory = fs.mkdtempSync(path.join(os.tmpdir(), "eca-image-tools-"));
const source = path.join(directory, "root");
fs.mkdirSync(source);
fs.writeFileSync(path.join(source, "marker"), "image tools work\n");

const rootfs = path.join(directory, "rootfs.ext4");
createRootfsImage(findMke2fs(), rootfs, source, "eca-test");
const contents = execFileSync("debugfs", ["-R", "cat /marker", rootfs], {encoding: "utf8"});
assert.equal(contents, "image tools work\n");

const initramfs = path.join(directory, "initramfs.cpio.lz4");
createInitramfs(source, initramfs);
const archive = execFileSync("lz4", ["-d", "-c", initramfs]);
const entries = execFileSync("cpio", ["-it"], {input: archive, encoding: "utf8"});
assert.ok(entries.split("\n").includes("marker") || entries.split("\n").includes("./marker"));

const {arch} = JSON.parse(fs.readFileSync(process.env.ECA_GONDOLIN_BUILD_CONFIG, "utf8"));
assert.match(execFileSync(`qemu-system-${arch}`, ["--version"], {encoding: "utf8"}), /QEMU/);
process.exit(0);
