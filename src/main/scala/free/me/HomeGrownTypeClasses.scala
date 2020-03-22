package free.me
import scala.language.higherKinds
object HomeGrownTypeClasses {

  trait Monoid[A] {
    def combine(x: A, y: A): A
    def identity(a: A): A
  }
  implicit object MonoidImplicit extends Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y
    override def identity(a: Int): Int = a
  }
  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }
  // applies to type constructors (F[_])
  trait Functor[F[_]] {
    def map[A, B](xs: F[A])(f: A => B): F[B]
  }
  object Functor {
    implicit object ListFunctor extends Functor[List] {
      override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
    }
    implicit object OptionFunctor extends Functor[Option] {
      override def map[A, B](xs: Option[A])(f: A => B): Option[B] = xs.map(f)
    }
  }
  trait ApplicativeFunctor[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }
  object ApplicativeFunctor {
    implicit object ListApplicativeFunctor extends ApplicativeFunctor[List] {
      override def pure[A](a: A): List[A] = List(a)
      override def zip[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
      override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
    }
    implicit object OptionApplicativeFunctor extends ApplicativeFunctor[Option] {
      override def pure[A](a: A): Option[A] = Option(a)
      override def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
        case (Some(a), Some(b)) => Option((a, b))
        case (None, _) => None
        case (_, None) => None
      }
      override def map[A, B](xs: Option[A])(f: A => B): Option[B] = xs.map(f)
    }
    def get[F[_] : ApplicativeFunctor]: ApplicativeFunctor[F] = implicitly[ApplicativeFunctor[F]]
  }
  def zipV(x: Option[String], y: Option[Int]): Option[(String, Int)] = {
    ApplicativeFunctor.get[Option].zip[String, Int](x, y)
  }
  trait Monad[F[_]] extends ApplicativeFunctor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  // todo : Fistful of monads
  // todo: Reader, Writer &  State Monads
  // todo: fp cons ds illustration.
  sealed trait VList[+A]
  case object Nil extends VList[Nothing]
  case class Cons[+A](h: A, tail: VList[A]) extends VList[A]
  val x: Cons[Int] = Cons[Int](1, Nil)
  val y: Cons[Int] = Cons[Int](2, x) // composing stuff yay!
  val x1: List[Set[Option[String]]] =
    List.empty[String]
      .map(e => Option(e))
      .map(e => Set(e)) // map applies a function that takes pure values
  // and return pure values which are then boxed within the type for which this functor typeclass applies to
  // however flatmap
  val x2: List[Option[String]] =
  List.empty[String]
    .flatMap(e => Some(s"$e:2"))
    .map(e => Option(e))
}