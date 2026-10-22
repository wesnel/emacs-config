// Stands in for the Gondolin SDK when tests/offline-run.test.mjs runs the
// runner. Instead of booting a VM, `exec' writes what the runner asked for to
// $STUB_REPORT.
import fs from "node:fs";

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

export class VM {
  constructor(options) {
    this.options = options;
  }

  async start() {
    if (process.env.STUB_FAIL_START) throw new Error("stub start failure");
  }

  exec(command, options) {
    const {mounts} = this.options.vfs;

    fs.writeFileSync(
      process.env.STUB_REPORT,
      JSON.stringify({
        command,
        cwd: options.cwd,
        env: this.options.env,
        mounts: Object.fromEntries(
          Object.entries(mounts).map(([guest, p]) => [guest, {root: p.root, readonly: !!p.readonly}]),
        ),
        hooksCreated: hooks !== null,
        httpHooks: "httpHooks" in this.options,
        dns: this.options.dns ?? null,
        tcp: this.options.tcp ?? null,
        sandbox: this.options.sandbox ?? null,
        memory: this.options.memory ?? null,
        cpus: this.options.cpus ?? null,
      }),
    );

    return {
      session: {stdoutPipe: null, stderrPipe: null},
      result: Promise.resolve({exitCode: 0}),
    };
  }

  async close() {}
}
