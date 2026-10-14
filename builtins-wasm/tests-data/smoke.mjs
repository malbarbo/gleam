// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

// Smoke test of the built module through the memory ABI, doing what the
// generated glue does. Usage, from builtins-wasm:
//   cargo build --release --target wasm32-unknown-unknown
//   node tests-data/smoke.mjs target/wasm32-unknown-unknown/release/builtins_wasm.wasm

import { readFileSync } from "node:fs";
import { WASI } from "node:wasi";
import assert from "node:assert/strict";

const wasi = new WASI({ version: "preview1", returnOnExit: true });
const module = await WebAssembly.compile(readFileSync(process.argv[2]));
const instance = await WebAssembly.instantiate(module, wasi.getImportObject());
const x = instance.exports;
wasi.initialize({ exports: { memory: x.memory, _initialize() {} } });

const enc = new TextEncoder();
const dec = new TextDecoder("utf-8", { fatal: true });
const view = () => new DataView(x.memory.buffer);
const alloc = (size) => x.cabi_realloc(0, 0, 8, size);

function str(s) {
  const bytes = enc.encode(s);
  const ptr = alloc(bytes.length);
  new Uint8Array(x.memory.buffer, ptr, bytes.length).set(bytes);
  return [ptr, bytes.length];
}
const readStr = (at) => {
  const v = view();
  const ptr = v.getUint32(at, true);
  const len = v.getUint32(at + 4, true);
  return dec.decode(new Uint8Array(x.memory.buffer, ptr, len));
};
const readList = (at, size, read) => {
  const v = view();
  const ptr = v.getUint32(at, true);
  const n = v.getUint32(at + 4, true);
  return Array.from({ length: n }, (_, i) => read(ptr + i * size));
};
const readResult = (at, read) =>
  view().getUint32(at, true) === 0 ? { ok: read(at + 8) } : { error: null };

// Calls `f` the way the glue does: mark, lower, call with a return area, lift, release.
function call(f, args, retSize, lift) {
  const mark = x._arena_mark();
  const lowered = args.flatMap((a) => (typeof a === "function" ? a() : [a]));
  const ret = retSize ? alloc(retSize) : undefined;
  const r = retSize ? (f(...lowered, ret), lift(ret)) : f(...lowered);
  x._arena_release(mark);
  assert.equal(x._arena_mark(), mark);
  return r;
}

x._config(64, 64);
const S = (s) => () => str(s);

assert.deepEqual(
  call(x.string_split, [S("a,b,,c"), S(",")], 8, (r) => readList(r, 8, readStr)),
  ["a", "b", "", "c"],
);
assert.deepEqual(
  call(x.string_to_graphemes, [S("aé🇧🇷")], 8, (r) => readList(r, 8, readStr)),
  ["a", "é", "🇧🇷"],
);
assert.equal(call(x.string_trim, [S(" \t gleam \n")], 8, readStr), "gleam");
assert.equal(call(x.string_uppercase, [S("straße")], 8, readStr), "STRASSE");
assert.equal(call(x.string_replace, [S("a.b.c"), S("."), S("--")], 8, readStr), "a--b--c");
assert.equal(call(x.string_slice, [S("gleam"), -2n, 2n], 8, readStr), "am");
assert.equal(call(x.string_slice, [S("gleam"), 1n, (1n << 32n) + 1n], 8, readStr), "leam");
assert.equal(call(x.string_drop_end, [S("gleam"), (1n << 32n) + 1n], 8, readStr), "");
assert.equal(call(x.string_drop_start, [S("gleam"), (1n << 32n) + 1n], 8, readStr), "");
assert.equal(call(x.string_length,[S("é🇧🇷")]), 2n);
assert.equal(call(x.string_contains, [S("gleam"), S("lea")]), 1);
assert.equal(call(x.string_inspect, [S('a"\n\u001b')], 8, readStr), '"a\\"\\n\\u{001B}"');
assert.deepEqual(
  call(x.string_split_once, [S("k=v=w"), S("=")], 24, (r) =>
    readResult(r, (p) => [readStr(p), readStr(p + 8)]),
  ),
  { ok: ["k", "v=w"] },
);
assert.deepEqual(call(x.string_first, [S("")], 16, (r) => readResult(r, readStr)), { error: null });
assert.deepEqual(
  call(x.string_to_utf_codepoints, [S("aé🎉")], 8, (r) => readList(r, 8, (p) => view().getUint32(p, true))),
  [0x61, 0xe9, 0x1f389],
);
const codepoints = () => {
  const ptr = alloc(24);
  [0x67, 0xe9, 0x1f389].forEach((c, i) => view().setUint32(ptr + i * 8, c, true));
  return [ptr, 3];
};
assert.equal(call(x.string_from_utf_codepoints, [codepoints], 8, readStr), "gé🎉");

assert.deepEqual(call(x.int_parse, [S("-42")], 16, (r) => readResult(r, (p) => view().getBigInt64(p, true))), { ok: -42n });
assert.deepEqual(call(x.int_parse, [S("4_2")], 16, (r) => readResult(r, (p) => view().getBigInt64(p, true))), { error: null });
assert.deepEqual(call(x.int_base_parse, [S("ff"), 16n], 16, (r) => readResult(r, (p) => view().getBigInt64(p, true))), { ok: 255n });
assert.equal(call(x.int_to_base_string, [-255n, 16n], 8, readStr), "-FF");
assert.equal(call(x.int_to_string, [1234567890123n], 8, readStr), "1234567890123");
assert.equal(call(x.float_to_string, [1e21], 8, readStr), "1.0e21");
assert.equal(call(x.float_to_string, [0.1], 8, readStr), "0.1");
assert.deepEqual(call(x.float_parse, [S("1.5e3")], 16, (r) => readResult(r, (p) => view().getFloat64(p, true))), { ok: 1500 });
assert.deepEqual(call(x.float_parse, [S("1")], 16, (r) => readResult(r, (p) => view().getFloat64(p, true))), { error: null });
assert.equal(x.float_power(2, 10), 1024);
for (let i = 0; i < 1000; i++) {
  const r = x.float_random();
  assert.ok(r >= 0 && r < 1);
}

assert.equal(call(x.uri_percent_encode, [S("a b/é")], 8, readStr), "a%20b%2F%C3%A9");
assert.deepEqual(call(x.uri_percent_decode, [S("%C3%A9%4")], 16, (r) => readResult(r, readStr)), { ok: "é%4" });
assert.deepEqual(
  call(x.uri_parse_query, [S("a=1&&b+c=%41")], 16, (r) =>
    readResult(r, (p) => readList(p, 16, (q) => [readStr(q), readStr(q + 8)])),
  ),
  { ok: [["a", "1"], ["", ""], ["b c", "A"]] },
);

// raw exports
const [gp, gl] = str("éx");
assert.equal(x._grapheme_prefix_len(gp, gl, 0), 3);
assert.equal(x._grapheme_prefix_len(gp, 3, 0), -1);
assert.equal(x._grapheme_prefix_len(gp, 3, 1), 3);
const scratch = x._heap_base();
assert.ok(scratch >= x._arena_mark() && scratch % 16 === 0);
const n = x._float_repr(1e-7, scratch);
assert.equal(dec.decode(new Uint8Array(x.memory.buffer, scratch, n)), "1.0e-7");
assert.equal(dec.decode(new Uint8Array(x.memory.buffer, scratch, x._f64_to_str(-0.0, scratch))), "-0.0");
assert.equal(dec.decode(new Uint8Array(x.memory.buffer, scratch, x._i64_to_str(-12n, scratch))), "-12");
x._config(32, 32);
assert.equal(dec.decode(new Uint8Array(x.memory.buffer, scratch, x._float_repr(1 / 3, scratch))), "0.33333334");
assert.equal(dec.decode(new Uint8Array(x.memory.buffer, scratch, x._float_repr(Math.fround(0.1), scratch))), "0.1");
assert.deepEqual(call(x.int_parse, [S("2147483648")], 16, (r) => readResult(r, (p) => view().getBigInt64(p, true))), { error: null });
x._config(64, 64);

// growing the memory
const before = x.memory.buffer.byteLength;
const mark = x._arena_mark();
const big = alloc(before * 2);
assert.ok(x.memory.buffer.byteLength >= big + before * 2);
x._arena_release(mark);

console.log("smoke ok");
