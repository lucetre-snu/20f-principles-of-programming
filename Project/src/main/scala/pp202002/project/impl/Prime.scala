package pp202002.project.impl

object Prime {
  // Problem 4: implement nthPrime.
  // nthPrime should return a function that calculates n-th prime number.

  //  class Primes(val prime: Int, val primes: List[Int]) {
  //    def getNext: Primes = {
  //      val p = computeNextPrime(prime + 2)
  //      new Primes(p, primes ++ (p :: Nil))
  //    }
  //    def computeNextPrime(n: Int) : Int =
  //      if (primes.forall((p:Int) => n%p != 0)) n
  //      else computeNextPrime(n+2)
  //  }
  //  def nthPrime(n: Int): Int = {
  //    def go(primes: Primes, k: Int): Int =
  //      if (k < 2) primes.prime
  //      else go(primes.getNext, k - 1)
  //    if (n < 2) 2 else go(new Primes(3, List(3)), n)
  //  }
  //  nthPrime(10000)

  val nthPrime: String =
    """
      |(let
      |    ((def nthPrime (n)
      |        (let
      |            ((def go (primes k)
      |                (let
      |                    ((def getNext (p primes)
      |                        (match (nil? primes)
      |                            ((_)
      |                                (match (= (% p (fst primes)) 0)
      |                                    ((_) (app getNext p (snd primes)))
      |                                    ((_) (app getNext (+ p 2) primes))
      |                                )
      |                            )
      |                            ((_) p)
      |                        )
      |                    )
      |                    (val prime (fst primes)))
      |                    (match (< k 2)
      |                        ((_) (app go (cons (app getNext (+ prime 2) primes) primes) (- k 1)))
      |                        ((_) prime)
      |                    )
      |                )
      |            ))
      |            (match (< n 2)
      |                ((_) (app go (cons 3 nil) (- n 1)))
      |                ((_) 2)
      |            )
      |        )
      |    ))
      |nthPrime)
      |""".stripMargin
}