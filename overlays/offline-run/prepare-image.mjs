import {ensureImage, managedImageOptions} from "./image.mjs";

try {
  if (!process.env.OFFLINE_RUN_GONDOLIN_LIB) throw new Error("OFFLINE_RUN_GONDOLIN_LIB is unset");
  const {buildAssets, verifyAssets} = await import(process.env.OFFLINE_RUN_GONDOLIN_LIB);
  const image = await ensureImage({...managedImageOptions(), buildAssets, verifyAssets});
  process.stderr.write(`offline-run: prepared image ${image}\n`);
} catch (error) {
  process.stderr.write(`offline-run: image preparation failed: ${error.message}\n`);
  process.exitCode = 1;
}
