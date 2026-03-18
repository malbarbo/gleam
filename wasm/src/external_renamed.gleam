type I32 {}

@external(webassembly, "builtins", "I32")
type Wasm32

@external(webassembly, "builtins", "_int_to_i32")
fn i32(value: Int) -> I32

@external(webassembly, "builtins", "_int_to_i32")
fn wasm32(value: Int) -> Wasm32

pub fn main() {
  echo i32(10)
  echo wasm32(20)
  echo [i32(1), i32(2)]
  echo [wasm32(3), wasm32(4)]
  echo #(i32(5), wasm32(6))
  True
}
