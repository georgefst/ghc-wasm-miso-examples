import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

(async () => {
  const args = [];
  const env = [
    `EXAMPLE=${globalThis.example}`,
    // `GHCRTS=-S`,
  ];
  const fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
    ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
  ];
  const options = { debug: false };
  const wasi = new WASI(args, env, fds, options);

  const instance_exports = {};
  const { instance } = await WebAssembly.instantiateStreaming(
    fetch("bin.wasm"),
    {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
    },
  );
  Object.assign(instance_exports, instance.exports);

  wasi.initialize(instance);
  instance.exports.hs_init(0, 0);
  instance.exports.hs_start();
})();
