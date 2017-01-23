package prob_monad.explicit

import scalaz.{Functor, Monad}

case class Explicit[A](list: List[(A, Double)])

trait ExplicitInstances {

  implicit val explicitInstance = new Functor[Explicit] with Monad[Explicit] {

    def point[A](a: => A): Explicit[A] = Explicit((a, 1.0) :: Nil)

    def bind[A, B](fa: Explicit[A])(f: A => Explicit[B]): Explicit[B] = {
      Explicit(
        for {
          (valueA, probA) <- fa.list
          (valueB, probB) <- f(valueA).list
        } yield (valueB, probA * probB)
      )
    }

    override def map[A, B](fa: Explicit[A])(f: A => B): Explicit[B] = {
      Explicit(fa.list.map {
        case (value, prob) => (f(value), prob)
      })
    }

  }

}

object ExplicitInstances extends ExplicitInstances
