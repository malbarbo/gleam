// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// Runs the WebAssembly modules that Gleam generates, in Node.js, Deno or Bun,
// with the functions they import: WASI and the JavaScript modules of their
// externals.

import { readFileSync, writeSync } from "node:fs";
import process from "node:process";
import {
  BitArray,
  Error as GleamError,
  List,
  Ok,
  UtfCodepoint,
  isEqual,
  makeError,
  toList,
} from "./prelude.mjs";

// The fields of the error of kind `kind` of the JavaScript target, from its
// `details`.
function errorFields(kind, details) {
  const expression = ([kind, value, start, end]) =>
    kind === "unevaluated" ? { kind, start, end } : { kind, value, start, end };
  if (kind === "let_assert") {
    const [value, start, end, pattern_start, pattern_end] = details;
    return { value, start, end, pattern_start, pattern_end };
  }
  if (kind === "assert") {
    const [start, end, expression_start, assertKind, ...rest] = details;
    let fields = { kind: assertKind };
    if (assertKind === "binary_operator") {
      const [operator, left, right] = rest;
      fields = { ...fields, operator, left: expression(left), right: expression(right) };
    } else if (assertKind === "function_call") {
      fields.arguments = rest[0].map(expression);
    } else {
      fields.expression = expression(rest[0]);
    }
    return { ...fields, start, end, expression_start };
  }
  return {};
}

// Thrown by `proc_exit`, to stop running the module.
export class Exit extends Error {
  constructor(code) {
    super(`exit with code ${code}`);
    this.code = code;
  }
}

// Instantiates the module at `url` (a `file:` URL). The functions of the host
// that it imports from a JavaScript module, declared with
// `@external(webassembly, "./file.mjs", "name")`, are the exports of that
// module. They receive and return the values of the JavaScript target, which
// the module converts with the functions of `gleam:js`.
export async function instantiate(url) {
  let memory;
  // Grows the memory to have `size` bytes from `address`, and gives them.
  const bytesAt = (address, size) => {
    const missing = address + size - memory.buffer.byteLength;
    if (missing > 0) memory.grow(Math.ceil(missing / 65536));
    return new Uint8Array(memory.buffer, address, size);
  };
  // The JavaScript functions of the closures, and the closures of the
  // JavaScript functions, see `function`.
  const functions = new WeakMap();
  const closures = new WeakMap();
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();
  const js = {
    number: (value) => value,
    to_number: (value) => Number(value),
    boolean: (value) => value !== 0,
    truthy: (value) => (value ? 1 : 0),
    undefined: () => undefined,
    codepoint: (value) => new UtfCodepoint(value),
    codepoint_value: (value) => value.value,
    to_string: (value) => (typeof value === "string" ? value : String(value)),
    string_from_memory: (address, size) =>
      decoder.decode(new Uint8Array(memory.buffer, address, size)),
    string_into_memory(value, address) {
      const string = js.to_string(value);
      // At most 3 bytes for each UTF-16 code unit.
      return encoder.encodeInto(string, bytesAt(address, string.length * 3)).written;
    },
    bit_array_from_memory: (address, byteSize, size, offset) =>
      new BitArray(
        new Uint8Array(memory.buffer.slice(address, address + byteSize)),
        size,
        offset,
      ),
    bit_array_into_memory(value, address) {
      if (!(value instanceof BitArray)) throw new TypeError(`Not a BitArray: ${value}`);
      bytesAt(address, value.rawBuffer.length).set(value.rawBuffer);
      return value.rawBuffer.length;
    },
    bit_array_offset: (value) => value.bitOffset,
    bit_array_size: (value) => value.bitSize,
    array_new: () => [],
    array_push(array, item) {
      array.push(item);
    },
    array_get: (array, index) => array[index],
    array_length: (array) => array.length,
    list_from_array: (array) => toList(array),
    list_to_array(value) {
      if (!(value instanceof List)) throw new TypeError(`Not a List: ${value}`);
      return value.toArray();
    },
    ok: (value) => new Ok(value),
    error: (value) => new GleamError(value),
    result_is_ok(value) {
      if (value instanceof Ok) return 1;
      if (value instanceof GleamError) return 0;
      throw new TypeError(`Not a Result: ${value}`);
    },
    result_value: (value) => value[0],
    function(closure, trampoline) {
      let f = functions.get(closure);
      if (f === undefined) {
        f = (...args) => trampoline(closure, ...args);
        // The arity of the function, which the trampoline takes after the
        // closure, as `string.inspect` shows it.
        Object.defineProperty(f, "length", { value: trampoline.length - 1 });
        js.register(closure, f);
      }
      return f;
    },
    closure_of: (f) => closures.get(f) ?? null,
    register(closure, f) {
      functions.set(closure, f);
      closures.set(f, closure);
    },
    call: (f, ...args) => f(...args),
    kind(value, dict) {
      if (typeof value === "number") return Number.isInteger(value) ? 1 : 2;
      if (typeof value === "string") return 3;
      if (typeof value === "boolean") return 4;
      if (value === undefined) return 5;
      if (value instanceof BitArray) return 6;
      if (value instanceof List) return 7;
      if (Array.isArray(value)) return 8;
      if (dict !== null && value instanceof dict) return 9;
      return 0;
    },
    entries: (fold, dict) =>
      fold(dict, [], (entries, key, value) => (entries.push(key, value), entries)),
    // The error of a `panic`, `todo`, `let assert` or `assert`, as on
    // JavaScript, with the fields of its `details` (see `panic.rs`).
    panic(kind, file, module, fn, line, message, details) {
      let extra = errorFields(kind, details);
      throw makeError(kind, file, module, Number(line), fn, message, extra);
    },
    unknown_variant(value) {
      throw new TypeError(`Not a variant of the custom type: ${inspect(value)}`);
    },
    equal: (a, b) => (isEqual(a, b) ? 1 : 0),
    hash,
    inspect: (value) => inspect(value),
  };
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
  const imports = { wasi_snapshot_preview1: wasi, "gleam:js": js };
  for (const { module: name } of WebAssembly.Module.imports(module)) {
    if (!(name in imports)) {
      // A module of a package is relative to the program, like on JavaScript,
      // where a module of the runtime, like `node:process`, is not.
      imports[name] = await import(/\.m?js$/.test(name) ? new URL(name, url) : name);
    }
  }
  const instance = await WebAssembly.instantiate(module, imports);
  memory = instance.exports.memory;
  return instance;
}

// A hash of `value` consistent with `isEqual`: of its text when it is not an
// object, and of the name of its class otherwise.
function hash(value) {
  const text =
    typeof value === "object" && value !== null
      ? (Object.getPrototypeOf(value)?.constructor?.name ?? "")
      : String(value);
  let hash = 0;
  for (let i = 0; i < text.length; i++) {
    hash = (Math.imul(hash, 31) + text.charCodeAt(i)) | 0;
  }
  return hash;
}

// The text of `value`, as `string.inspect` of the JavaScript target, with
// the objects in `seen` being shown.
function inspect(value, seen = new Set()) {
  if (value === true) return "True";
  if (value === false) return "False";
  if (value === null) return "//js(null)";
  if (value === undefined) return "Nil";
  if (typeof value === "string") return JSON.stringify(value);
  if (typeof value === "bigint" || Number.isSafeInteger(value)) return value.toString();
  if (typeof value === "number") return Number.isFinite(value) ? `${value}` : `//js(${value})`;
  if (value instanceof RegExp) return `//js(${value})`;
  if (value instanceof Date) return `//js(Date("${value.toISOString()}"))`;
  if (value instanceof globalThis.Error) return `//js(${value})`;
  if (typeof value === "function") {
    const args = Array.from({ length: value.length }, (_, i) => String.fromCharCode(97 + i));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  if (seen.has(value)) return "//js(circular reference)";
  seen.add(value);
  let text;
  if (Array.isArray(value)) {
    text = `#(${value.map((item) => inspect(item, seen)).join(", ")})`;
  } else {
    const name = Object.getPrototypeOf(value)?.constructor?.name || "Object";
    const props = Object.keys(value).map(
      (key) => `${inspect(key, seen)}: ${inspect(value[key], seen)}`,
    );
    const body = props.length ? ` ${props.join(", ")} ` : "";
    text = `//js(${name === "Object" ? "" : name + " "}{${body}})`;
  }
  seen.delete(value);
  return text;
}

// Runs the `main` function of the module at `url` and exits. The error of a
// `panic`, `todo`, `let assert` or `assert` is shown as in a program that
// does not run in JavaScript.
export async function run(url) {
  const instance = await instantiate(url);
  try {
    instance.exports._start();
  } catch (error) {
    if (error?.gleam_error !== undefined) {
      writeSync(2, `${error.gleam_error} at ${error.file}:${error.line}\n  ${error.message}\n`);
      process.exit(1);
    }
    if (!(error instanceof Exit)) throw error;
    process.exit(error.code);
  }
}
