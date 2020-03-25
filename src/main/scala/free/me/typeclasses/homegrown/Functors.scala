package free.me.typeclasses.homegrown

import scala.language.higherKinds
object Functors {
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
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
  }
  // as long as a functor implicit is in the scope we can map any of F[_] type over.
  implicit class FunctorPimp[F[_] : Functor, A](f: F[A]) {
    def map[B](fab: A => B): F[B] = Functor[F].map[A, B](f)(fab)
  }
}
