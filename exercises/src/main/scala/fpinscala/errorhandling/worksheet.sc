import fpinscala.errorhandling._

// option

def Try[A](a: => A): Option[A] = {
  try Some(a)
  catch { case e: Exception => None}
}

Option.map2(Some(1), Some(2))(_ + _)

Option.sequence(List(Some(1), Some(2), None, Some(4)))
Option.sequence(List(Some(1), Some(2), Some(3), Some(4)))
Option.sequenceViaTraverse(List(Some(1), Some(2), None, Some(4)))
Option.sequenceViaTraverse(List(Some(1), Some(2), Some(3), Some(4)))


Option.traverse(List("1", "2", "hello", "4"))(s => Try(s.toInt))
Option.traverse(List("1", "2", "3", "4"))(s => Try(s.toInt))
Option.traverseViaSequence(List("1", "2", "hello", "4"))(s => Try(s.toInt))
Option.traverseViaSequence(List("1", "2", "3", "4"))(s => Try(s.toInt))

// either

Right(1).map2(Right(2))(_ + _)

Either.sequence(List(Right(1), Right(2), Left("bad"), Right(4)))
Either.sequence(List(Right(1), Right(2), Right(3), Right(4)))

Either.traverse(List("1", "2", "hello", "4"))(s => Either.Try(s.toInt))
Either.traverse(List("1", "2", "3", "4"))(s => Either.Try(s.toInt))

Either.traverse_1(List("1", "2", "hello", "dog"))(s => Either.Try(s.toInt))
Either.traverse_1(List("1", "2", "3", "4"))(s => Either.Try(s.toInt))