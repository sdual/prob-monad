package prob_monad.explicit

import prob_monad.distribution.discrete.CoinDistribution
import prob_monad.distribution.discrete.CoinDistributionInstances._

import scalaz.Scalaz._

sealed trait Coin
case object Heads extends Coin
case object Tails extends Coin

object CoinToss extends App {

  def coin: CoinDistribution[Coin] = CoinDistribution(Heads -> 0.5 :: Tails -> 0.5 :: Nil)
  def loadedCoin: CoinDistribution[Coin] = CoinDistribution(Heads -> 0.1 :: Tails -> 0.9 :: Nil)

  val result = for {
    a <- coin
    b <- coin
    c <- loadedCoin
  } yield {a == Tails && b == Tails && c == Tails }

  println(result)
}
