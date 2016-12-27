package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case e: Left[E] => e
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this.map(f) match {
   case e: Left[E] => e
   case Right(r) => r
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case e: Left[E] => b
   case r => r
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(a => b.map(f(a, _)))

  def zip[EE >: E, B](b: Either[EE, B]): Either[List[EE], (A, B)] = {
    this match {
      case Left(e) =>
        b match {
          case Left(eb) =>
            Left(e :: eb :: Nil)
          case _ =>
            Left(e :: Nil)
        }
      case Right(r) =>
        b match {
          case Left(eb) =>
            Left(eb :: Nil)
          case Right(rb) =>
            Right((r, rb))
        }
    }
  }
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es.foldRight[Either[E, List[B]]](Right(Nil))((l, r) => f(l).map2(r)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es.foldRight[Either[E,List[A]]](Right(Nil))((l, r) => l.map2(r)(_ :: _))

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