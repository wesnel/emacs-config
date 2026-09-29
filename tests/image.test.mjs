import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {ensureImage} from "../overlays/eca-gondolin/image.mjs";

const fixture = (t) => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "eca-image-test-"));
  t.after(() => fs.rmSync(root, {recursive: true, force: true}));
  const configPath = path.join(root, "config.json");
  fs.writeFileSync(configPath, JSON.stringify({arch: "aarch64", distro: "alpine"}));
  let builds = 0;
  const options = {
    configPath,
    builderId: "gondolin-v1",
    cacheDirectory: path.join(root, "images"),
    buildAssets: async (_config, {outputDir}) => {
      builds++;
      const assets = {kernel: "kernel", initramfs: "initramfs", rootfs: "rootfs"};
      for (const file of Object.values(assets)) fs.writeFileSync(path.join(outputDir, file), "asset");
      fs.writeFileSync(path.join(outputDir, "manifest.json"), JSON.stringify({assets}));
    },
    verifyAssets: () => true,
  };
  return {options, builds: () => builds};
};

test("reuses a complete image without invoking the builder", async (t) => {
  const {options, builds} = fixture(t);
  const first = await ensureImage(options);
  assert.equal(await ensureImage(options), first);
  assert.equal(builds(), 1);
});

test("configuration, architecture and builder changes select new images", async (t) => {
  const {options, builds} = fixture(t);
  const first = await ensureImage(options);
  const builderChange = await ensureImage({...options, builderId: "gondolin-v2"});
  fs.writeFileSync(options.configPath, JSON.stringify({arch: "aarch64", distro: "alpine", env: {TEST: "1"}}));
  const configChange = await ensureImage(options);
  fs.writeFileSync(options.configPath, JSON.stringify({arch: "x86_64", distro: "alpine"}));
  const archChange = await ensureImage(options);
  assert.equal(new Set([first, builderChange, configChange, archChange]).size, 4);
  assert.equal(builds(), 4);
});

test("failed builds and failed verification are not cached", async (t) => {
  const {options, builds} = fixture(t);
  await assert.rejects(ensureImage({...options, buildAssets: async () => {throw new Error("download failed");}}), /download failed/);
  assert.deepEqual(fs.readdirSync(options.cacheDirectory), []);
  await assert.rejects(ensureImage({...options, verifyAssets: () => false}), /verification failed/);
  assert.deepEqual(fs.readdirSync(options.cacheDirectory), []);
  const image = await ensureImage(options);
  assert.ok(fs.existsSync(path.join(image, ".ready")));
  assert.equal(builds(), 2);
});

test("missing assets trigger a rebuild", async (t) => {
  const {options, builds} = fixture(t);
  const image = await ensureImage(options);
  fs.unlinkSync(path.join(image, "rootfs"));
  assert.equal(await ensureImage(options), image);
  assert.ok(fs.existsSync(path.join(image, "rootfs")));
  assert.equal(builds(), 2);
});

test("concurrent builds publish a complete image at the same path", async (t) => {
  const {options} = fixture(t);
  const [a, b] = await Promise.all([ensureImage(options), ensureImage(options)]);
  assert.equal(a, b);
  assert.ok(fs.existsSync(path.join(a, ".ready")));
  assert.equal(fs.readdirSync(options.cacheDirectory).filter((name) => name.startsWith(".building-")).length, 0);
});
