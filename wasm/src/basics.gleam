pub const true = True

pub const false = False

pub const myint = 10

pub const myfloat = 2.0

pub const mystring = "string"

pub const mylist = [1, 2, 3]

pub const mytuple = #(10, 1.0, False, "other", [1, 2])

pub fn main() {
  bool()
  int()
  float()
  string()
  list()
  tuple()
  functions()
}

fn bool() {
  let assert True = true
  let assert False = false
  not()
  and()
  or()
}

fn not() {
  let assert True = !False
  let assert False = !True
}

fn and() {
  let assert False = False && False
  let assert False = False && True
  let assert False = True && False
  let assert True = True && True
  let assert False = False && panic
}

fn or() {
  let assert False = False || False
  let assert True = False || True
  let assert True = True || False
  let assert True = True || True
  let assert True = True || panic
}

fn int() {
  let assert 10 = myint
  sum_int()
  sub_int()
  mul_int()
  rem_int()
  div_int()
  neg_int()
  cmp_int()
}

fn float() {
  let assert 2.0 = myfloat
  sum_float()
  sub_float()
  mul_float()
  div_float()
  cmp_float()
}

fn sum_int() {
  let assert 10 = 3 + 6 + 1
}

fn sub_int() {
  let assert 8 = 4 - 6 - -10
}

fn mul_int() {
  let assert 120 = 3 * 4 * 10
}

fn rem_int() {
  let assert 1 = 10 % 3
  let assert 1 = 10 % -3
  let assert -1 = -10 % 3
  let assert -1 = -10 % -3
}

fn div_int() {
  let assert 0 = 10 / 0
  let assert 5 = 10 / 2
  let assert 2 = 10 / 4
  let assert -2 = 10 / -4
  let assert -2 = -10 / 4
  let assert 2 = -10 / -4
}

pub fn neg_int() {
  let assert -10 = -{ 10 }
  let assert 10 = -{ -10 }
}

fn cmp_int() {
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

fn sum_float() {
  let assert 10.0 = 3.0 +. 6.0 +. 1.0
}

fn sub_float() {
  let assert 8.0 = 4.0 -. 6.0 -. -10.0
}

fn mul_float() {
  let assert 120.0 = 3.0 *. 4.0 *. 10.0
}

fn div_float() {
  let assert 0.0 = 10.0 /. 0.0
  let assert 5.0 = 10.0 /. 2.0
  let assert 2.5 = 10.0 /. 4.0
  let assert -2.5 = 10.0 /. -4.0
  let assert -2.5 = -10.0 /. 4.0
  let assert 2.5 = -10.0 /. -4.0
}

fn cmp_float() {
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

fn string() {
  let assert "string" = mystring
  concat_string()
  cmp_string()
}

fn concat_string() {
  let assert "ab cd" = "ab" <> " " <> "cd"
  let assert "string" = mystring <> ""
  let assert "string" = "" <> mystring
  let assert "string-string" = mystring <> "-" <> "string"
}

fn cmp_string() {
  let assert True = "abc" == "ab" <> "c"
  let assert False = "ab" == "cd" <> "a"
  let assert True = "ab" != "cd" <> "a"
  let assert False = "ab" != "a" <> "b"
}

fn list() {
  let assert [1, 2, 3] = mylist
  let a = [2, 1]
  let b = [1, 2, ..a]
  let assert [1, b, 2, a] = b
  let assert 3 = a + b
  cmp_list()
}

fn cmp_list() {
  // Int
  let empty = []
  let assert True = [] == empty
  let assert False = [1] == empty
  let assert False = [] != empty
  let assert True = [1] != empty
  let assert True = [1, 2, 3] == [1, 2, 3, ..empty]
  let assert False = [1, 2, 3] == [1, 2, ..empty]
  let assert False = [1, 2, 3] == [1, 2, 3, 4, ..empty]
  let assert False = [1, 2, 3] != [1, 2, 3, ..empty]
  let assert True = [1, 2, 3] != [1, 2, ..empty]
  let assert True = [1, 2, 3] != [1, 2, 3, 4, ..empty]
  // Float
  let empty = []
  let assert True = [] == empty
  let assert False = [1.0] == empty
  let assert False = [] != empty
  let assert True = [1.0] != empty
  let assert True = [1.0, 2.0, 3.0] == [1.0, 2.0, 3.0, ..empty]
  let assert False = [1.0, 2.0, 3.0] == [1.0, 2.0, ..empty]
  let assert False = [1.0, 2.0, 3.0] == [1.0, 2.0, 3.0, 4.0, ..empty]
  let assert False = [1.0, 2.0, 3.0] != [1.0, 2.0, 3.0, ..empty]
  let assert True = [1.0, 2.0, 3.0] != [1.0, 2.0, ..empty]
  let assert True = [1.0, 2.0, 3.0] != [1.0, 2.0, 3.0, 4.0, ..empty]
  // String
  let empty = []
  let assert True = [] == empty
  let assert False = ["1.0"] == empty
  let assert False = [] != empty
  let assert True = ["1.0"] != empty
  let assert True = ["1.0", "2.0", "3.0"] == ["1.0", "2.0", "3.0", ..empty]
  let assert False = ["1.0", "2.0", "3.0"] == ["1.0", "2.0", ..empty]
  let assert False =
    ["1.0", "2.0", "3.0"] == ["1.0", "2.0", "3.0", "4.0", ..empty]
  let assert False = ["1.0", "2.0", "3.0"] != ["1.0", "2.0", "3.0", ..empty]
  let assert True = ["1.0", "2.0", "3.0"] != ["1.0", "2.0", ..empty]
  let assert True =
    ["1.0", "2.0", "3.0"] != ["1.0", "2.0", "3.0", "4.0", ..empty]
  // List(String)
  let empty = []
  let assert True = [] == empty
  let assert False = [["1.0"]] == empty
  let assert False = [] != empty
  let assert True = [["1.0"]] != empty
  let assert True =
    [["1.0"], ["2.0", "3.0"]] == [["1.0"], ["2.0", "3.0"], ..empty]
  let assert False = [["1.0"], ["2.0", "3.0"]] == [["1.0"], ["2.0"], ..empty]
  let assert False =
    [["1.0"], ["2.0", "3.0"]] == [["1.0"], ["2.0", "3.0", "4.0"], ..empty]
  let assert False =
    [["1.0"], ["2.0", "3.0"]] != [["1.0"], ["2.0", "3.0"], ..empty]
  let assert True = [["1.0"], ["2.0", "3.0"]] != [["1.0"], ["2.0"], ..empty]
  let assert True =
    [["1.0"], ["2.0", "3.0"]] != [["1.0"], ["2.0", "3.0", "4.0"], ..empty]
}

fn tuple() {
  let assert #(10, 1.0, False, "other", [1, 2]) = mytuple
}

fn functions() {
  scope(5)
  let assert 11 = call(inc, 10)
  let assert 2 = local_function()(1)
  mix_params(10, 2.0, "c")
}

fn scope(a: Int) {
  let assert 5 = a
  let b = {
    let a = 2
    let assert 5 = 10 / a
    a + 1
  }
  let assert 5 = a
  let assert 3 = b
}

fn call(f: fn(Int) -> Int, a: Int) -> Int {
  f(a)
}

fn inc(a) {
  a + 1
}

fn local_function() -> fn(Int) -> Int {
  let f = fn(c) { c + 1 }
  f
}

fn mix_params(a: Int, b: Float, c: String) {
  // a = 10
  // b = 2.0
  // c = "c"
  let assert 10 = a
  let assert 4 = {
    let a = a / 4
    let b = 1
    let c = 1
    a + b + c
  }
  let assert 10 = a

  let assert 2.0 = b
  let assert 8.0 = {
    let b = b *. 3.0
    let a = 1.0
    let c = 1.0
    a +. b +. c
  }
  let assert 2.0 = b

  let assert "c" = c
  let assert "-a-b-c-" = {
    let c = "-" <> c <> "-"
    let a = "-a"
    let b = "-b"
    a <> b <> c
  }
  let assert "c" = c
  True
}
