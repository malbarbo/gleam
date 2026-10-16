// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// Runs the WebAssembly modules that Gleam generates, in Node.js, Deno or Bun,
// with the functions they import: WASI and the JavaScript modules of their
// externals.

import { readFileSync, writeSync } from "node:fs";
import process from "node:process";

// Thrown by `proc_exit`, to stop running the module.
export class Exit extends Error {
  constructor(code) {
    super(`exit with code ${code}`);
    this.code = code;
  }
}

// Instantiates the module at `url` (a `file:` URL). The functions of the host
// that it imports from a JavaScript module, declared with
// `@external(webassembly, "./file.mjs", "name")`, are the exports of that module.
export async function instantiate(url) {
  let memory;
  const wasi = {
    fd_write(fd, iovs, iovs_len, nwritten) {
      const view = new DataView(memory.buffer);
      let written = 0;
      for (let i = 0; i < iovs_len; i++) {
        const ptr = view.getUint32(iovs + i * 8, true);
        const len = view.getUint32(iovs + i * 8 + 4, true);
        written += writeSync(fd, new Uint8Array(memory.buffer, ptr, len));
      }
      view.setUint32(nwritten, written, true);
      return 0;
    },
    proc_exit(code) {
      throw new Exit(code);
    },
    random_get(buf, len) {
      crypto.getRandomValues(new Uint8Array(memory.buffer, buf, len));
      return 0;
    },
  };
  // The JavaScript strings of a module compiled with `string = "JSString"`
  // are the strings of the host, operated by its builtins, and its string
  // literals are imported from the module "'" with the literal as the name.
  // A module that does not import them is not affected.
  const module = await WebAssembly.compile(readFileSync(url), {
    builtins: ["js-string"],
    importedStringConstants: "'",
  });
  // The builtins and the string literals that the runtime provides are not
  // imports, so these are left only when it does not have them.
  const builtins = ["wasm:js-string", "'"];
  if (WebAssembly.Module.imports(module).some((i) => builtins.includes(i.module))) {
    throw new Error(
      'This module was compiled with string = "JSString" and needs a runtime ' +
        "with the JavaScript string builtins of WebAssembly, " +
        "like Node.js 24, Deno 2.9 or Bun 1.4",
    );
  }
  const imports = { wasi_snapshot_preview1: wasi };
  for (const { module: name } of WebAssembly.Module.imports(module)) {
    if (!(name in imports) && /\.m?js$/.test(name)) {
      imports[name] = await import(new URL(name, url));
    }
  }
  const instance = await WebAssembly.instantiate(module, imports);
  memory = instance.exports.memory;
  return instance;
}

// Runs the `main` function of the module at `url` and exits.
export async function run(url) {
  const instance = await instantiate(url);
  try {
    instance.exports._start();
  } catch (error) {
    if (!(error instanceof Exit)) throw error;
    process.exit(error.code);
  }
}
