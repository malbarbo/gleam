use super::run_ok;

#[test]
fn int_const() {
    run_ok(
        r#"
const myint = 10

pub fn main() {
    let assert 10 = myint
}
"#,
    );
}

#[test]
fn int_sum() {
    run_ok(
        r#"
pub fn main() {
    let assert 10 = 3 + 6 + 1
}
"#,
    );
}

#[test]
fn int_sub() {
    run_ok(
        r#"
pub fn main() {
    let assert 8 = 4 - 6 - -10
}
"#,
    );
}

#[test]
fn int_mul() {
    run_ok(
        r#"
pub fn main() {
    let assert 120 = 3 * 4 * 10
}
"#,
    );
}

#[test]
fn int_rem() {
    run_ok(
        r#"
pub fn main() {
    let assert 1 = 10 % 3
    let assert 1 = 10 % -3
    let assert -1 = -10 % 3
    let assert -1 = -10 % -3
}
"#,
    );
}

#[test]
fn int_div() {
    run_ok(
        r#"
pub fn main() {
    let assert 0 = 10 / 0
    let assert 5 = 10 / 2
    let assert 2 = 10 / 4
    let assert -2 = 10 / -4
    let assert -2 = -10 / 4
    let assert 2 = -10 / -4
}
"#,
    );
}

#[test]
fn int_negation() {
    run_ok(
        r#"
pub fn main() {
    let assert -10 = -{ 10 }
    let assert 10 = -{ -10 }
}
"#,
    );
}

#[test]
fn int_comparison() {
    run_ok(
        r#"
pub fn main() {
    let assert True = 5 == 4 + 1
    let assert False = 5 == 3 + 1
    let assert True = 5 != 3 + 1
    let assert False = 5 != 4 + 1
    let assert True = 5 < 5 + 1
    let assert False = 5 < 4 + 1
    let assert True = 5 <= 4 + 1
    let assert False = 6 <= 4 + 1
    let assert True = 6 > 4 + 1
    let assert False = 5 > 4 + 1
    let assert True = 5 >= 4 + 1
    let assert False = 5 >= 5 + 1
}
"#,
    );
}

#[test]
fn float_const() {
    run_ok(
        r#"
const myfloat = 2.0

pub fn main() {
    let assert 2.0 = myfloat
}
"#,
    );
}

#[test]
fn float_sum() {
    run_ok(
        r#"
pub fn main() {
    let assert 10.0 = 3.0 +. 6.0 +. 1.0
}
"#,
    );
}

#[test]
fn float_sub() {
    run_ok(
        r#"
pub fn main() {
    let assert 8.0 = 4.0 -. 6.0 -. -10.0
}
"#,
    );
}

#[test]
fn float_mul() {
    run_ok(
        r#"
pub fn main() {
    let assert 120.0 = 3.0 *. 4.0 *. 10.0
}
"#,
    );
}

#[test]
fn float_div() {
    run_ok(
        r#"
pub fn main() {
    let assert 0.0 = 10.0 /. 0.0
    let assert 5.0 = 10.0 /. 2.0
    let assert 2.5 = 10.0 /. 4.0
    let assert -2.5 = 10.0 /. -4.0
    let assert -2.5 = -10.0 /. 4.0
    let assert 2.5 = -10.0 /. -4.0
}
"#,
    );
}

#[test]
fn float_comparison() {
    run_ok(
        r#"
pub fn main() {
    let assert True = 5.0 == 4.0 +. 1.0
    let assert False = 5.0 == 3.0 +. 1.0
    let assert True = 5.0 != 3.0 +. 1.0
    let assert False = 5.0 != 4.0 +. 1.0
    let assert True = 5.0 <. 5.0 +. 1.0
    let assert False = 5.0 <. 4.0 +. 1.0
    let assert True = 5.0 <=. 4.0 +. 1.0
    let assert False = 6.0 <=. 4.0 +. 1.0
    let assert True = 6.0 >. 4.0 +. 1.0
    let assert False = 5.0 >. 4.0 +. 1.0
    let assert True = 5.0 >=. 4.0 +. 1.0
    let assert False = 5.0 >=. 5.0 +. 1.0
}
"#,
    );
}

#[test]
fn even_odd() {
    run_ok(
        r#"
pub fn par(a: Int) -> Bool {
  case a {
    _ if a < 0 -> impar(a + 1)
    _ if a == 0 -> True
    _ -> impar(a - 1)
  }
}

pub fn impar(a: Int) -> Bool {
  case a {
    _ if a < 0 -> par(a + 1)
    _ if a == 0 -> False
    _ -> par(a - 1)
  }
}

pub fn main() {
  assert par(-3) == False
  assert par(-2) == True
  assert par(-1) == False
  assert par(0) == True
  assert par(1) == False
  assert par(2) == True
  assert par(3) == False
  assert impar(-3) == True
  assert impar(-2) == False
  assert impar(-1) == True
  assert impar(0) == False
  assert impar(1) == True
  assert impar(2) == False
  assert impar(3) == True
}
"#,
    );
}

#[test]
fn max_two() {
    run_ok(
        r#"
fn maximo(a, b) {
  case a > b {
    True -> a
    False -> b
  }
}

pub fn main() {
  assert maximo(1, 2) == 2
  assert maximo(3, 1) == 3
  assert maximo(5, 5) == 5
}
"#,
    );
}

#[test]
fn max_three() {
    run_ok(
        r#"
pub fn maximo3(a: Int, b: Int, c: Int) -> Int {
  case a >= b {
    True ->
      case a >= c {
        True -> a
        False -> c
      }
    False ->
      case b >= c {
        True -> b
        False -> c
      }
  }
}

pub fn main() {
  assert maximo3(8, 5, 2) == 8
  assert maximo3(4, 6, 1) == 6
  assert maximo3(6, 6, 7) == 7
}
"#,
    );
}

#[test]
fn three_digits() {
    run_ok(
        r#"
pub fn tres_digitos(n: Int) -> Bool {
  100 <= n && n <= 999
}

pub fn main() {
  assert tres_digitos(99) == False
  assert tres_digitos(100) == True
  assert tres_digitos(999) == True
  assert tres_digitos(1000) == False
}
"#,
    );
}

#[test]
fn prime() {
    run_ok(
        r#"
pub fn primo(n: Int) -> Bool {
  num_divisors(n, n) == 2
}

fn num_divisors(n: Int, a: Int) -> Int {
  case a {
    _ if a <= 0 -> 0
    _ if n % a == 0 -> 1 + num_divisors(n, a - 1)
    _ -> num_divisors(n, a - 1)
  }
}

pub fn main() {
  assert primo(1) == False
  assert primo(2) == True
  assert primo(3) == True
  assert primo(4) == False
  assert primo(5) == True
  assert primo(6) == False
  assert primo(7) == True
  assert primo(8) == False
}
"#,
    );
}

#[test]
fn perfect_number() {
    run_ok(
        r#"
pub fn perfeito(n: Int) -> Bool {
  n > 0 && n == soma_divisores(n, n - 1)
}

pub fn soma_divisores(n: Int, d: Int) -> Int {
  case d <= 0 {
    True -> 0
    False ->
      case n % d == 0 {
        True -> d + soma_divisores(n, d - 1)
        False -> soma_divisores(n, d - 1)
      }
  }
}

pub fn main() {
  assert perfeito(1) == False
  assert perfeito(6) == True
  assert perfeito(28) == True
  assert perfeito(29) == False
}
"#,
    );
}

#[test]
fn exponentiation() {
    run_ok(
        r#"
pub fn exponencial(a: Float, n: Int) -> Result(Float, Nil) {
  case a == 0.0 && n == 0 || n < 0 {
    True -> Error(Nil)
    False -> Ok(exponencial_(a, n))
  }
}

pub fn exponencial_(a: Float, n: Int) -> Float {
  case n {
    0 -> 1.0
    _ -> a *. exponencial_(a, n - 1)
  }
}

pub fn main() {
  assert exponencial(0.0, -2) == Error(Nil)
  assert exponencial(0.0, 0) == Error(Nil)
  assert exponencial(0.0, 2) == Ok(0.0)
  assert exponencial(3.0, -2) == Error(Nil)
  assert exponencial(3.0, 0) == Ok(1.0)
  assert exponencial(3.0, 2) == Ok(9.0)
}
"#,
    );
}

#[test]
fn list_sum() {
    run_ok(
        r#"
pub fn soma(lst: List(Int)) -> Int {
  case lst {
    [] -> 0
    [primeiro, ..resto] -> primeiro + soma(resto)
  }
}

pub fn soma2(lst: List(Int)) -> Int {
  soma_loop(lst, 0)
}

pub fn soma_loop(lst: List(Int), acc: Int) -> Int {
  case lst {
    [] -> acc
    [primeiro, ..resto] -> soma_loop(resto, acc + primeiro)
  }
}

pub fn main() {
  assert soma([]) == 0
  assert soma([4]) == 4
  assert soma([7, 1]) == 8
  assert soma2([]) == 0
  assert soma2([4]) == 4
  assert soma2([7, 1]) == 8
}
"#,
    );
}

#[test]
fn max_list() {
    run_ok(
        r#"
pub fn maximo(lst: List(Int)) -> Result(Int, Nil) {
  case lst {
    [] -> Error(Nil)
    [primeiro, ..resto] ->
      case maximo(resto) {
        Error(Nil) -> Ok(primeiro)
        Ok(maximo_resto) -> Ok(max(primeiro, maximo_resto))
      }
  }
}

fn max(a, b) {
  case a > b {
    True -> a
    False -> b
  }
}

pub fn main() {
  assert maximo([]) == Error(Nil)
  assert maximo([2]) == Ok(2)
  assert maximo([2, 1]) == Ok(2)
  assert maximo([2, 1, 5]) == Ok(5)
}
"#,
    );
}

#[test]
fn palindrome() {
    run_ok(
        r#"
pub fn palindromo(lst: List(Int)) -> Bool {
  lst == reverse(lst, [])
}

pub fn reverse(lst: List(a), r: List(a)) -> List(a) {
  case lst {
    [] -> r
    [first, ..rest] -> reverse(rest, [first, ..r])
  }
}

pub fn main() {
  assert palindromo([]) == True
  assert palindromo([2]) == True
  assert palindromo([1, 2]) == False
  assert palindromo([3, 3]) == True
  assert palindromo([3, 7, 3]) == True
  assert palindromo([3, 7, 3, 3]) == False
}
"#,
    );
}

#[test]
fn prefix() {
    run_ok(
        r#"
pub fn prefixo(lsta: List(a), lstb: List(a)) -> Bool {
  case lsta, lstb {
    [], _ -> True
    _, [] -> False
    [a, ..restoa], [b, ..restob] -> a == b && prefixo(restoa, restob)
  }
}

pub fn main() {
  assert prefixo([], []) == True
  assert prefixo([], [3, 4]) == True
  assert prefixo([3, 4], []) == False
  assert prefixo([3, 4], [3, 4]) == True
  assert prefixo([3, 4], [3, 4, 6, 8]) == True
  assert prefixo([3, 4], [3, 5]) == False
  assert prefixo([3, 4, 5], [3, 4]) == False
}
"#,
    );
}

#[test]
fn external_type_i64() {
    run_ok(
        r#"
type I64 {}

@external(webassembly, "builtins", "_int_to_i64")
fn to_i64(value: Int) -> I64

@external(webassembly, "builtins", "_i64_to_int")
fn from_i64(value: I64) -> Int

pub fn main() {
  assert from_i64(to_i64(42)) == 42
  assert from_i64(to_i64(-1)) == -1
  0
}
"#,
    );
}

#[test]
fn external_type_f32() {
    run_ok(
        r#"
type F32 {}

@external(webassembly, "builtins", "_float_to_f32")
fn to_f32(value: Float) -> F32

@external(webassembly, "builtins", "_f32_to_float")
fn from_f32(value: F32) -> Float

pub fn main() {
  let x = from_f32(to_f32(3.14))
  // F32 loses precision, so check approximately
  assert x >. 3.13
  assert x <. 3.15
  0
}
"#,
    );
}

#[test]
fn external_type_f64() {
    run_ok(
        r#"
type F64 {}

@external(webassembly, "builtins", "_float_to_f64")
fn to_f64(value: Float) -> F64

@external(webassembly, "builtins", "_f64_to_float")
fn from_f64(value: F64) -> Float

pub fn main() {
  assert from_f64(to_f64(3.14)) == 3.14
  0
}
"#,
    );
}
