package free.me
import cats.kernel.Semigroup
import scala.language.higherKinds
object HomeGrownTypeClasses {
  /**
   * A type class (a class of types) that has a capability to combine
   * values of same types `A`
   *
   * @tparam A operand/combination operand type.
   */
  trait SemiGroup[A] {
    def combine(x: A, y: A): A
  }
  /**
   * A specialized semigroup that applies to `A` types with identity.
   *
   * @tparam A type parameter `A`
   */
  trait Monoid[A] extends Semigroup[A] {
    def identity(a: A): A
  }
  implicit object MonoidImplicit extends Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y
    override def identity(a: Int): Int = a
  }
  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }
  /**
   * A typeclass (class of types) that can be mapped over with a function that takes pure values
   * (we posit values of shape F[_] are effect full values and `A` alone
   * are pure values/also sometimes called UnBoxed values - i.e. values
   * that do not rely on any context. People in FP call this Effectual values).
   * and returns pure values. Exhibiting this behavior allows us to use pure functions (A => B) on any
   * boxed type/ effectful type.
   *
   * @tparam F type constructor.
   */
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
  /**
   * Applicative Functor is a specialized functor typeclass (class of types) - that have capability
   * to be zipped over such that -
   * given fa and fb effectful values we can combine those to produce f (a,b) effectful value.
   *
   * @tparam F type constructor.
   */
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
  /**
   * Specialized applicative functor which exhibits capability to bind >>= or flatMap in scala or
   * sequence effectful types/computations. e.g.
   * given fa we can apply a fn which takes a (pure value in fa) and gives back effectual fb which
   * then can be bound to b => fc and so on.
   *
   * @tparam F type constructor.
   */
  trait Monad[F[_]] extends ApplicativeFunctor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  // todo : Fistful of monads
  // todo: Reader, Writer &  State Monads
  //<~~  random FP stuff ~~>
  // todo: fp cons ds illustration.
  // todo: Trampolines
  // todo: tail recursion http://blog.higher-order.com/assets/trampolines.pdf
  // todo: TailCalls.tailcall()
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