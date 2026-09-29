import {ensureImage, managedImageOptions} from "./image.mjs";

try {
  if (!process.env.ECA_GONDOLIN_LIB) throw new Error("ECA_GONDOLIN_LIB is unset");
  const {buildAssets, verifyAssets} = await import(process.env.ECA_GONDOLIN_LIB);
  const image = await ensureImage({...managedImageOptions(), buildAssets, verifyAssets});
  process.stderr.write(`eca-gondolin: prepared image ${image}\n`);
} catch (error) {
  process.stderr.write(`eca-gondolin: image preparation failed: ${error.message}\n`);
  process.exitCode = 1;
}
