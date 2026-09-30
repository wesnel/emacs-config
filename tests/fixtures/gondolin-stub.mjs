// Stands in for the Gondolin SDK when tests/gondolin-wrapper.test.mjs runs
// the wrapper. Instead of booting a VM, `exec' writes what the wrapper asked
// for to $STUB_REPORT, reading the staged config while it still exists.
import fs from "node:fs";
import path from "node:path";

let hooks = null;

export class RealFSProvider {
  constructor(root) {
    this.root = root;
  }
}

export class ReadonlyProvider {
  constructor(inner) {
    this.root = inner.root;
    this.readonly = true;
  }
}

export const createHttpHooks = (options) => {
  hooks = options;
  return {httpHooks: {}, env: {}};
};

export const buildAssets = () => {
  throw new Error("the stub cannot build images");
};

export const verifyAssets = () => false;

const tree = (root, prefix = "") =>
  Object.fromEntries(
    fs.readdirSync(path.join(root, prefix), {withFileTypes: true}).flatMap((entry) => {
      const name = path.join(prefix, entry.name);
      return entry.isDirectory()
        ? Object.entries(tree(root, name))
        : [[name, fs.readFileSync(path.join(root, name), "utf8")]];
    }),
  );

export class VM {
  constructor(options) {
    this.options = options;
  }

  async start() {
    if (process.env.STUB_FAIL_START) throw new Error("stub start failure");
  }

  exec(command, options) {
    const {mounts} = this.options.vfs;
    const config = mounts["/root/.config/eca"];
    const probes = JSON.parse(process.env.STUB_PROBES || "{}");

    fs.writeFileSync(
      process.env.STUB_REPORT,
      JSON.stringify({
        command,
        cwd: options.cwd,
        env: this.options.env,
        mounts: Object.fromEntries(
          Object.entries(mounts).map(([guest, p]) => [guest, {root: p.root, readonly: !!p.readonly}]),
        ),
        config: config ? tree(config.root) : null,
        allowedHosts: hooks.allowedHosts ?? null,
        allowedInternalHosts: hooks.allowedInternalHosts,
        ipAllowed: (probes.ip ?? []).map((info) => hooks.isIpAllowed(info)),
        rewritten: (probes.urls ?? []).map((url) => hooks.onRequest(new Request(url))?.url ?? null),
      }),
    );

    return {
      session: {stdoutPipe: null, stderrPipe: null},
      result: Promise.resolve({exitCode: 0}),
    };
  }

  async close() {}
}
