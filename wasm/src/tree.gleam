type Tree(a) {
  Empty
  Node(value: a, left: Tree(a), right: Tree(a))
}

pub fn main() {
  let a = Empty
  let b = Node(10, Node(20, a, Empty), Empty)
  assert a == Empty
  assert b == b
  assert a != b
  echo a
  echo b
  assert height(a) == -1
  assert height(b) == 1
  assert height(Node("b", Empty, Empty)) == 0
  assert prune_left(b) == Node(10, Empty, Empty)
  assert swap(b) == Node(10, Empty, Node(20, a, Empty))
  0
}

fn height(t: Tree(a)) -> Int {
  case t {
    Empty -> -1
    Node(_, Empty, Empty) -> 0
    Node(..) -> 1 + max(height(t.left), height(t.right))
  }
}

fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

fn prune_left(t: Tree(a)) -> Tree(a) {
  case t {
    Empty -> Empty
    Node(..) -> Node(..t, left: Empty)
  }

}
fn swap(t: Tree(a)) -> Tree(a) {
  case t {
    Empty -> Empty
    Node(..) -> Node(..t, left: t.right, right: t.left)
  }
}
