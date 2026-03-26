type Point(a, b) {
  Point(x: Int, y: a, z: b)

}

const p = Point(10, "a", 2.0)

pub fn main() {
  assert p == Point(10, "a", 2.0)
  let p = Point(10, 20.0, 30)
  echo p
  assert p == Point(10, 20.0, 30)
  let assert Point(10, z: a, y: b) = p
  assert a == 30
  assert b == 20.0
  assert Point(..p) == Point(10, 20.0, 30)
  assert Point(..Point(1, "2", 3), z: [2]) == Point(1, "2", [2])
  True
}
