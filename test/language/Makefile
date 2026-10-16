.PHONY: build
build: clean erlang nodejs deno

.PHONY: clean
clean:
	rm -rf build

.PHONY: erlang
erlang:
	@echo test/language on Erlang
	cargo run --quiet -- test --target erlang

.PHONY: nodejs
nodejs:
	@echo test/language on JavaScript with Node
	cargo run --quiet -- test --target javascript --runtime nodejs

.PHONY: deno
deno:
	@echo test/language on JavaScript with Deno
	cargo run --quiet -- test --target javascript --runtime deno

.PHONY: bun
bun:
	@echo test/language on JavaScript with Bun
	cargo run --quiet -- test --target javascript --runtime bun

# The packages of Hex don't run on WebAssembly yet, so the tests run in a copy
# of this project with the checkouts of the standard library and gleeunit
# that do.
STDLIB ?= $(abspath ../../../gleam-stdlib)
GLEEUNIT ?= $(abspath ../../../gleeunit)
WEBASSEMBLY_PROJECT = build/webassembly-project

.PHONY: webassembly
webassembly:
	@echo test/language on WebAssembly with Node
	rm -rf $(WEBASSEMBLY_PROJECT)
	mkdir -p $(WEBASSEMBLY_PROJECT)
	cp -R src test $(WEBASSEMBLY_PROJECT)
	sed -e 's|^gleam_stdlib = .*|gleam_stdlib = { path = "$(STDLIB)" }|' \
		-e 's|^gleeunit = .*|gleeunit = { path = "$(GLEEUNIT)" }|' \
		gleam.toml > $(WEBASSEMBLY_PROJECT)/gleam.toml
	cd $(WEBASSEMBLY_PROJECT) && cargo run --quiet -- test --target webassembly
