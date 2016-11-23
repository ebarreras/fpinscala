import fpinscala.errorhandling._
import fpinscala.errorhandling.Option._

def Try[A](a: => A): Option[A] = {
  try Some(a)
  catch { case e: Exception => None}
}

map2(Some(1), Some(2))(_ + _)

sequence(List(Some(1), Some(2), None, Some(4)))
sequence(List(Some(1), Some(2), Some(3), Some(4)))
sequenceViaTraverse(List(Some(1), Some(2), None, Some(4)))
sequenceViaTraverse(List(Some(1), Some(2), Some(3), Some(4)))


traverse(List("1", "2", "hello", "4"))(s => Try(s.toInt))
traverse(List("1", "2", "3", "4"))(s => Try(s.toInt))
traverseViaSequence(List("1", "2", "hello", "4"))(s => Try(s.toInt))
traverseViaSequence(List("1", "2", "3", "4"))(s => Try(s.toInt))