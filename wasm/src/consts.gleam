const i1 = 10
const i2 = i1

const f1 = 20.0
const f2 = f1

const b1 = True
const b2 = b1

const l1 = [b1]
const l2 = l1

const s1 = "abc"
const s2 = s1

const o1 = Ok(s1)
const o2 = o1

const e1 = Error(f1)
const e2 = e1

pub fn main() {
  assert i1 == i2
  assert f1 == f2
  assert b1 == b2
  assert l1 == l2
  assert s1 == s2
  assert o1 == o2
  assert e1 == e2
}
