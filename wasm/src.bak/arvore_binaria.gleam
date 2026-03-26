

pub type Arvore(a) {
  Vazia
  No(valor: a, esq: Arvore(a), dir: Arvore(a))
}

/// Determina o número de nós folhas de *r*.
pub fn num_folhas(r: Arvore(a)) -> Int {
  case r {
    Vazia -> 0
    No(_, Vazia, Vazia) -> 1
    No(_, esq, dir) -> num_folhas(esq) + num_folhas(dir)
  }
}

pub fn num_folhas_examples() {
  //     t4  3
  //       /   \
  //  t3  4     7  t2
  //     / \   / \
  //    3   2 8   9  t1
  //             /
  //        t0  10

  let t0 = No(10, Vazia, Vazia)
  let t1 = No(9, t0, Vazia)
  let t2 = No(7, No(8, Vazia, Vazia), t1)
  let t3 = No(4, No(3, Vazia, Vazia), No(2, Vazia, Vazia))
  let t4 = No(3, t3, t2)

  eq(num_folhas(Vazia), 0)
  eq(num_folhas(t0), 1)
  eq(num_folhas(t1), 1)
  eq(num_folhas(t2), 2)
  eq(num_folhas(t3), 2)
  eq(num_folhas(t4), 4)
}

/// Devolve a altura de *r*. A altura de uma árvore binária é a distância da
/// raiz a seu descendente mais afastado. Uma árvore com um único nó tem altura
/// 0 e uma árvore vazia tem altura -1.
pub fn altura(r: Arvore(a)) -> Int {
  case r {
    Vazia -> -1
    No(_, esq, dir) -> 1 + max(altura(esq), altura(dir))
  }
}

pub fn altura_examples() {
  //     t4  3
  //       /   \
  //  t3  4     7  t2
  //     /     / \
  //    3     8   9  t1
  //             /
  //        t0  10

  let t0 = No(10, Vazia, Vazia)
  let t1 = No(9, t0, Vazia)
  let t2 = No(7, No(8, Vazia, Vazia), t1)
  let t3 = No(4, No(3, Vazia, Vazia), Vazia)
  let t4 = No(3, t3, t2)

  eq(altura(Vazia), -1)
  eq(altura(t0), 0)
  eq(altura(t1), 1)
  eq(altura(t2), 2)
  eq(altura(t3), 1)
  eq(altura(t4), 3)
}

fn max(a, b) {
  case a > b {
    True -> a
    False -> b
  }
}
