# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 The Gleam contributors

"""Checks the built module against what the code generator can handle (see
ABI.md): every export is its own function, and no function reachable from an
export makes an indirect call. Needs `wasm-objdump` (wabt). Usage:

    python3 tests-data/check_wasm.py target/wasm32-unknown-unknown/release/builtins_wasm.wasm
"""

import collections
import re
import subprocess
import sys


def objdump(*args):
    return subprocess.run(["wasm-objdump", *args, sys.argv[1]], capture_output=True, text=True, check=True).stdout


exports = re.findall(r' - func\[(\d+)\] <([^>]*)> -> "([^"]+)"', objdump("-x", "-j", "Export"))
ok = True
for index, name, export in exports:
    if name != export:
        print(f"export {export} is function {name}")
        ok = False

callees = collections.defaultdict(set)
indirect = set()
func = None
for line in objdump("-d").splitlines():
    if m := re.search(r"func\[(\d+)\] <[^>]*>:", line):
        func = m.group(1)
    elif "call_indirect" in line:
        indirect.add(func)
    elif m := re.search(r"\bcall (\d+)\b", line):
        callees[func].add(m.group(1))

seen = {index for index, _, _ in exports}
stack = list(seen)
while stack:
    for g in callees[stack.pop()]:
        if g not in seen:
            seen.add(g)
            stack.append(g)
for f in sorted(seen & indirect, key=int):
    print(f"func[{f}] makes an indirect call and is reachable from an export")
    ok = False

print(f"{len(exports)} exports, {len(seen)} reachable functions: {'ok' if ok else 'FAILED'}")
sys.exit(0 if ok else 1)
