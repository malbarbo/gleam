type I32 {}

@external(webassembly, "builtins", "_repr_int")
fn int_str(value: Int, address: I32) -> I32

@external(webassembly, "builtins", "_repr_float")
fn float_str(value: Float, address: I32) -> I32

@external(webassembly, "builtins", "_int_to_i32")
fn i32(value: Int) -> I32

@external(webassembly, "builtins", "_i32_to_int")
fn int(value: I32) -> Int

@external(webassembly, "builtins", "_memory_to_string")
fn str(address: I32, length: I32) -> String

@external(webassembly, "builtins", "_heap_base")
fn heap_base() -> I32

fn i32_add(a: I32, b: Int) -> I32 {
  i32(int(a) + b)
}

pub fn main() {
  let addr = heap_base()
  assert int_str(1234, addr) == i32(4)
  assert float_str(56.789, i32_add(addr, 4)) == i32(6)
  assert str(addr, i32(10)) == "123456.789"
  echo i32(10)
  echo [i32(1), i32(2)]
  echo #(i32(3), "hello")
  True
}
