package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => f(a)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(a) => Right(a)
 }

 def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   flatMap (a => eb.map(b => f(a, b)))

  def map2ViaFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- b
    } yield f(a,b)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil: List[B]): Either[E, List[B]])((a, acc) => acc.map2(f(a))((bs, b) => b :: bs))

  def sequence[E,A](as: List[Either[E,A]]): Either[E,List[A]] =
    traverse(as)(x => x)

  // accumulates errors
  def traverse_1[E,A,B](as: List[A])(f: A => Either[E,B]): Either[List[E], List[B]] =
    as.foldRight(Right(Nil: List[B]): Either[List[E], List[B]])((a, acc) => f(a) match {
      case Left(e) => acc match {
        case Left(ee) => Left(e :: ee)
        case Right(_) => Left(List(e))
      }
      case Right(a) => acc match {
        case Left(_) => acc
        case Right(aa) => Right(a :: aa)
      }
    })

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}