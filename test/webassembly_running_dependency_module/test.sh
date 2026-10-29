#!/bin/sh

# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2026 The Gleam contributors

set -eu

GLEAM_COMMAND=${GLEAM_COMMAND:-"cargo run --quiet --"}

g() {
	echo "Running: $GLEAM_COMMAND $@"
	$GLEAM_COMMAND "$@"
}

expect_output() {
	expected=$1
	shift
	output=$(g "$@" 2>&1) || true
	echo "$output"
	case "$output" in
	*"$expected"*) ;;
	*)
		echo "Expected the output to contain: $expected"
		exit 1
		;;
	esac
}

echo Resetting the build directory to get to a known state
rm -fr build

echo Running a module of the root package
expect_output '"hello from root"' run --target=webassembly

echo Running a module of a dependency
expect_output '"hello from dep"' run --module=dep --target=webassembly

echo
echo Success! 💖
echo
