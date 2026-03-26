type Tree(a) {
  Tree(lft: a, rgt: a)
}

fn soma(f: fn(a) -> Int, t: Tree(a)) -> Int {
  f(t.lft) + f(t.rgt)
}

// Calcula 2^n criando uma árvore cheia de profundidade n e contando suas folhas
fn loop(n: Int, v: a, contar: fn(a) -> Int) -> Int {
  case n {
    0 -> contar(v)
    _ -> {
      let sub = Tree(lft: v, rgt: v)
      loop(n - 1, sub, fn(t: Tree(a)) -> Int {
        soma(contar, t)
      })
    }
  }
}

pub fn pow2(n: Int) -> Int {
  loop(n, Nil, fn(_) { 1 })
}
