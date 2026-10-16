// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import process from "node:process";

export function halt(code) {
  process.exit(Number(code));
}

export function toDynamic(a) {
  return a;
}
