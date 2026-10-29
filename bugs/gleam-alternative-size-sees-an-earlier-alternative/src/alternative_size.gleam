// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// The `n` of `size(n)` is not bound by the second alternative before it is
// used, nor outside the case, but it is accepted: the type checker finds the
// `n` bound by the first alternative.
fn unbound(b: BitArray) -> Int {
  case b {
    <<n, 1>> | <<_:size(n), n>> -> n
    _ -> -1
  }
}

// Here `size(n)` should be the parameter `n`, but it is typed as the `n` of
// the first alternative, and the parameter is reported as unused.
fn shadowed(b: BitArray, n: Int) -> Int {
  case b {
    <<n, 1>> | <<_:size(n), n>> -> n
    _ -> -1
  }
}

pub fn main() {
  echo unbound(<<16, 0>>)
  echo shadowed(<<16, 0>>, 8)
}
