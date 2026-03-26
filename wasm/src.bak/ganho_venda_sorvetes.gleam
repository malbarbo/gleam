

const preco_venda = 10

const custo_manga = 6

const custo_uva = 7

const custo_morango = 8

/// O sabor de um sorvete vendido.
pub type Sabor {
  Manga
  Uva
  Morango
}

/// Calcula o ganho pela venda dos sorvetes de *lst*.
pub fn ganho_venda_sorvetes(lst: List(Sabor)) -> Int {
  case lst {
    [] -> 0
    [sabor, ..resto] -> {
      let ganho = case sabor {
        Manga -> preco_venda - custo_manga
        Uva -> preco_venda - custo_uva
        Morango -> preco_venda - custo_morango
      }
      ganho + ganho_venda_sorvetes(resto)
    }
  }
}

pub fn ganho_venda_sorvetes_examples() {
  eq(ganho_venda_sorvetes([]), 0)
  eq(ganho_venda_sorvetes([Manga]), 4)
  eq(ganho_venda_sorvetes([Uva, Manga]), 7)
  eq(ganho_venda_sorvetes([Morango, Uva, Manga]), 9)
  eq(ganho_venda_sorvetes([Manga, Morango, Uva, Manga]), 13)
}
