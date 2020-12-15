package pp202002.project.impl

object Prime {
  // Problem 4: implement nthPrime.
  // nthPrime should return a function that calculates n-th prime number.
  val nthPrime: String =
    """
      |(let
      |    ((def nthprime (n)
      |        (let
      |            ((def go (primes k)
      |                (let
      |                    ((def getNextPrime (candidate prevPrimes)
      |                        (match (nil? prevPrimes)
      |                            ((_)
      |                                (match (= (% candidate (fst prevPrimes)) 0)
      |                                    ((_) (app getNextPrime candidate (snd prevPrimes)))
      |                                    ((_) (app getNextPrime (+ candidate 2) primes))
      |                                )
      |                            )
      |                            ((_) candidate)
      |                        )
      |                    ) (val currentPrime (fst primes)))
      |
      |                    (match (< k 2)
      |                        ((_) (app go (cons (app getNextPrime (+ currentPrime 2) primes) primes) (- k 1)))
      |                        ((_) currentPrime)
      |                    )
      |                )
      |            ))
      |
      |            (match (< n 2)
      |                ((_) (app go (cons 3 nil) (- n 1)))
      |                ((_) 2)
      |            )
      |        )
      |    ))
      |nthprime)
      |""".stripMargin
}