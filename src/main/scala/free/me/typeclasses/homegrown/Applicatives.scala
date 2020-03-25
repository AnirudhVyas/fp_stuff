package free.me.typeclasses.homegrown

import free.me.typeclasses.homegrown.Functors.Functor
import Functors.Functor
import free.me.typeclasses.homegrown.typeclasses.homegrown.Functors.Functor

import scala.language.higherKinds
/**
 * Applicative Functor is a specialized functor typeclass (class of types) - that have capability
 * to be zipped over such that -
 * given fa and fb effect values we can combine those to produce f (a,b) effectful value.
 *
 */
object Applicatives {
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
    def apply[F[_] : ApplicativeFunctor]: ApplicativeFunctor[F] = implicitly[ApplicativeFunctor[F]]
  }
  def zipV(x: Option[String], y: Option[Int]): Option[(String, Int)] = {
    ApplicativeFunctor[Option].zip[String, Int](x, y)
  }
}