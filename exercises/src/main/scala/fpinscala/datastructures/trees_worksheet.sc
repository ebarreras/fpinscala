import fpinscala.datastructures.Tree._
import fpinscala.datastructures._

val tree = Branch(
  Branch(
    Branch(
      Leaf(1),
      Leaf(2)
    ),
    Branch(
      Leaf(3),
      Leaf(4)
    )
  ), Branch(
    Branch(
      Leaf(5),
      Leaf(6)
    ),
    Branch(
      Leaf(7),
      Leaf(8)
    )
  ))



size(tree)
fold(tree)(_ => 1)((l, r) => l + r + 1)

maximum(tree)
fold(tree)(a => a)((l, r) => l max r)

depth(tree)
fold(tree)(_ => 0)((l, r) => l max r + 1)

map(tree)(_ + 1)
fold(tree)(a => Leaf(a + 1): Tree[Int])((l, r) => Branch(l, r))
















