<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2026 The Gleam contributors
-->

# webassembly_running_dependency_module

This package is used to check that `gleam run -m <dependency-module>` works on
the WebAssembly target, whose programs are generated only for the modules of
the root package, unless one of a dependency is run.
