<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2026 The Gleam contributors
-->

# Bugs found outside the WebAssembly backend

Genuine bugs in the parts of the compiler the WebAssembly backend shares
with the other targets, found while working on the backend. Each subfolder
is a self-contained reproducer: a Gleam project, and a `README.md` with
the exact command that triggers the failure, the observed vs. expected
behaviour, and the source line responsible.

These are kept here rather than under `test/` because they assert the
behaviour of code the backend does not own; when one is fixed upstream it
becomes a test there and its folder goes.

Each entry's header carries a `**Reported:**` field --
`grep -c 'not reported' */README.md` is the "give it back upstream" queue.

| # | Bug | Component | Kind |
|---|-----|-----------|------|
| 1 | [the `size` of a pattern alternative sees the variables of an earlier one](gleam-alternative-size-sees-an-earlier-alternative/) | type checker | wrong program accepted (Erlang does not compile it, JavaScript throws) |
