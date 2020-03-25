package free.me.typeclasses.homegrown

import Applicatives.ApplicativeFunctor

import scala.language.higherKinds
object Monads {

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
}