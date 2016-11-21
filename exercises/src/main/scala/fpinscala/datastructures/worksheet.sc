import fpinscala.datastructures._
import fpinscala.datastructures.List._

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}

drop(List(1, 2, 3, 4, 5), 2)

dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3)

dropWhile(Nil, (x: Int) => x < 3)

init(List(1, 2, 3, 4, 5))

length(List(1, 2, 3, 4, 5))

map(List(1, 2, 3, 4, 5))(_ + "a")

mapViaFoldRight(List(1, 2, 3, 4, 5))(_ + "a")

reverse(List(1, 2, 3, 4, 5))

flatMap(List(1,2,3))(i => List(i,i))

filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)

filterViaFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0)

addElems(List(1, 2, 3, 4, 5), List(1, 2, 3))
addElems(List(1, 2, 3), List(1, 2, 3, 4, 5))

hasSubsequence(List(8, 1, 2, 3, 4, 5), List(1, 2, 3))
hasSubsequence(List(8, 1, 2, 6, 4, 5), List(1, 2, 3))








