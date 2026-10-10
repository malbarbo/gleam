// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

// Reads f64 bit patterns (hex, one per line) from stdin and prints the
// `float_to_string` of `gleam_stdlib.mjs` for each one. Used by the ignored
// test `num::tests::float_to_string_matches_node`.

import { readFileSync } from "node:fs";

function f(x) {
  const s = x.toString().replace("+", "");
  if (s.indexOf(".") >= 0) return s;
  const i = s.indexOf("e");
  return i >= 0 ? s.slice(0, i) + ".0" + s.slice(i) : s + ".0";
}

const bits = new BigUint64Array(1);
const float = new Float64Array(bits.buffer);
const out = [];
for (const line of readFileSync(0, "utf8").split("\n")) {
  if (!line) continue;
  bits[0] = BigInt("0x" + line);
  out.push(f(float[0]));
}
process.stdout.write(out.join("\n") + "\n");
