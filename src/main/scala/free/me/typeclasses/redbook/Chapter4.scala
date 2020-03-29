package free.me.typeclasses.redbook
/**
 * Chapter 4 solutions for red book.
 * refer to functional programming in scala book.
 */
object Chapter4 {
  import scala.{Either => _, Option => _}
  // 4.1
  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Just(a) => Just(f(a))
      case NothingM => NothingM
    }
    // this is taken from answer key -- couldn't figure it out by myself.
    // the trick is what do you want to get the result type.
    // i have map so far, if i do map(f) i get option[option]
    // how do i get option out of it, by doing a get.
    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(NothingM)
    def getOrElse[B >: A](default: => B): B = this match {
      case Just(a) => a
      case NothingM => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Just(_)).getOrElse(ob)
    def filter(f: A => Boolean): Option[A] = this match {
      case Just(a) => if (f(a)) Just(a) else NothingM
      case NothingM => NothingM
    }
  }
  case object NothingM extends Option[Nothing]
  case class Just[+A](a: A) extends Option[A]
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Just(_), NothingM) => NothingM
      case (NothingM, Just(_)) => NothingM
      case (Just(a), Just(b)) => Just(f(a, b))
    }
  }
}
