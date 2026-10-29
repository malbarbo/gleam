<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2026 The Gleam contributors
-->

# The `size` of a pattern alternative sees the variables of an earlier one

**Kind:** wrong program accepted -- the type checker accepts a program that
the Erlang target does not compile and the JavaScript target does not run.
**Component:** the type checker, `PatternTyper::bit_array_size`
(`compiler-core/src/type_/pattern.rs:1410`). A variable of `size(..)` is
looked up first in `self.variables`, the variables bound by the pattern.
Only the first alternative of a clause inserts its variables there
(`PatternMode::Initial`); the others are checked against them
(`PatternMode::Alternative`) and leave them in place. So in a later
alternative `size(n)` finds the `n` of the first one, even when that
alternative has not bound `n` yet, and even when an `n` outside the case
should be the one meant.
**Version:** `main` at `9bc9c7d6b` (Gleam 1.19.1).
**Reported:** not reported.

## Reproduce

```sh
gleam run --target erlang
# src/alternative_size.gleam:9: variable 'N' is unbound
# src/alternative_size.gleam:18: variable 'N@1' is unbound
gleam run --target javascript
# ReferenceError: n is not defined
```

`src/alternative_size.gleam` has two clauses
`<<n, 1>> | <<_:size(n), n>> -> n`. In `unbound` there is no other `n`, so
the expected answer is an unknown variable error; in `shadowed` there is a
parameter `n`, which the expected answer reads, but the compiler warns
that it is never used, because it typed `size(n)` as the `n` of the first
alternative. Both targets then generate a read of a variable that the
second alternative has not bound.

## Where it was met

The review of the WebAssembly backend found that the scope of a case
clause was shared by its alternatives, so an alternative saw the variables
bound by the ones before it that failed. The only program that can observe
that is this one, because every alternative binds the same variables
before the guard and the body read them. The backend now starts each
alternative from the scope of the case (the test
`webassembly::tests::bit_arrays::pattern_alternatives_scope`), so
`shadowed` reads the parameter, which is what the type checker should
decide, and `unbound` panics in the backend with "module value n should
be a constant or a function", which is what the type checker should
reject.
