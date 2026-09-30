import fs from "node:fs";
import path from "node:path";
import {createHash, randomUUID} from "node:crypto";

const ready = (directory) => {
  try {
    if (!fs.existsSync(path.join(directory, ".ready"))) return false;
    const manifest = JSON.parse(fs.readFileSync(path.join(directory, "manifest.json"), "utf8"));
    return ["kernel", "initramfs", "rootfs"].every((name) => {
      const file = manifest.assets?.[name];
      return typeof file === "string" && fs.statSync(path.join(directory, file)).size > 0;
    });
  } catch {
    return false;
  }
};

export async function ensureImage({configPath, builderId, cacheDirectory, buildAssets, verifyAssets}) {
  const configText = fs.readFileSync(configPath, "utf8");
  const config = JSON.parse(configText);
  const key = createHash("sha256").update(builderId).update("\0").update(configText).digest("hex");
  const image = path.join(cacheDirectory, `${config.arch}-${key}`);
  if (ready(image)) return image;

  fs.mkdirSync(cacheDirectory, {recursive: true});
  const staging = fs.mkdtempSync(path.join(cacheDirectory, ".building-"));
  try {
    await buildAssets(config, {
      outputDir: staging,
      configDir: path.dirname(configPath),
      verbose: true,
    });
    if (!verifyAssets(staging)) throw new Error("Gondolin image verification failed");
    fs.writeFileSync(path.join(staging, ".ready"), "");
    if (!ready(staging)) throw new Error("Gondolin image is missing required assets");

    // A concurrent build may already have published this same image.
    if (ready(image)) return image;
    if (fs.existsSync(image)) {
      fs.renameSync(image, `${image}.incomplete-${randomUUID()}`);
    }
    try {
      fs.renameSync(staging, image);
    } catch (error) {
      if (!ready(image)) throw error;
    }
    prune(cacheDirectory, path.basename(image));
    return image;
  } finally {
    fs.rmSync(staging, {recursive: true, force: true});
  }
}

// A build younger than this may belong to a concurrent activation.
const STALE_BUILD_MS = 24 * 60 * 60 * 1000;

// Runs only after a new image is published, which is when the previous one
// is superseded. A session still running on a removed image keeps its open
// files, so it is not disturbed.
function prune(cacheDirectory, keep, now = Date.now()) {
  for (const name of fs.readdirSync(cacheDirectory)) {
    if (name === keep) continue;

    const entry = path.join(cacheDirectory, name);
    const image = /^[a-z0-9_]+-[0-9a-f]{64}(\.incomplete-.+)?$/.test(name);
    let stale = false;

    if (name.startsWith(".building-")) {
      try {
        stale = now - fs.statSync(entry).mtimeMs > STALE_BUILD_MS;
      } catch {
        continue;
      }
    }

    if (image || stale) {
      fs.rmSync(entry, {recursive: true, force: true});
    }
  }
}

export function managedImageOptions(env = process.env) {
  for (const name of ["ECA_GONDOLIN_BUILD_CONFIG", "ECA_GONDOLIN_BUILDER_ID", "ECA_GONDOLIN_IMAGE_CACHE"]) {
    if (!env[name]) throw new Error(`${name} is unset`);
  }
  return {
    configPath: env.ECA_GONDOLIN_BUILD_CONFIG,
    builderId: env.ECA_GONDOLIN_BUILDER_ID,
    cacheDirectory: env.ECA_GONDOLIN_IMAGE_CACHE,
  };
}
