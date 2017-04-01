package prob_monad.distribution.discrete

import scalaz.{Functor, Monad}

case class CoinDistribution[A](list: List[(A, Double)])

trait CoinDistributionInstances {

  implicit val explicitInstance = new Functor[CoinDistribution] with Monad[CoinDistribution] {

    def point[A](a: => A): CoinDistribution[A] = CoinDistribution((a, 1.0) :: Nil)

    def bind[A, B](fa: CoinDistribution[A])(f: A => CoinDistribution[B]): CoinDistribution[B] = {
      CoinDistribution(
        for {
          (valueA, probA) <- fa.list
          (valueB, probB) <- f(valueA).list
        } yield (valueB, probA * probB)
      )
    }

    override def map[A, B](fa: CoinDistribution[A])(f: A => B): CoinDistribution[B] = {
      CoinDistribution(fa.list.map {
        case (value, prob) => (f(value), prob)
      })
    }

  }

}

object CoinDistributionInstances extends CoinDistributionInstances
