{
  def x = a
  def a = 1 + (2 + 3)
  val y = 1 + 2
  println(x)
  println(y)
}
{
  def x = 1
  println(x+2)
}
{
  def loop: Int = loop
  def one(x: Int, y: =>Int) = 1
  one(1+2, loop)
  // one(loop, 1+2)
}
{
  def callByValue(x: Int) = {
    println("x1 = " + x)
    println("x2 = " + x)
  }
  def callByName(x: =>Int) = {
    println("x1 = " + x)
    println("x2 = " + x)
  }

  callByValue({
    println("Calling by value!!")
    10
  })
  callByName({
    println("Calling by name!!")
    10
  })
}
{
  def loop: Int = loop

  val x = if(false) loop else false
  println(x)

  val y = if(true) true else loop
  println(y)
}
{
  def abs(x: Double) = if(x < 0) -x else x
  def isGoodEnough(guess: Double, x: Double) =
    abs(guess*guess - x)/x < 0.0001

  def improve(guess: Double, x: Double) =
    (guess + x/guess) / 2

  def sqrtIter(guess: Double, x: Double): Double =
    if(isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def sqrt(x: Double) = sqrtIter(1, x)

  println(sqrt(9))
}
{
  import scala.annotation.tailrec
  def abs(x: Double) = if(x < 0) -x else x

  def sqrt(x: Double) = {
    def isGoodEnough(guess: Double) =
      abs(guess*guess - x)/x < 0.0001

    def improve(guess: Double) =
      (guess + x/guess) / 2

    @tailrec
    def sqrtIter(guess: Double): Double =
      if(isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    sqrtIter(1)
  }

  println(sqrt(9))
}
{
  val t = 0
  def x = t+t
  val r = {
    val t = 10
    x+t
  }
  println(x)
}
{
  def f(c: Boolean, i: =>Int): Int = {
    lazy val iv = i
    if (c) 0
    else iv * iv * iv
  }
  f(true, { println ("ok"); 100+100+100+100})
  f(false, { println ("ok"); 100+100+100+100})

}
{
  import scala.annotation.tailrec

  def printStar(n: Int) = {
    @tailrec
    def printStarIter_j(j: Int, res: String): String = {
      if(j <= 0) res else printStarIter_j(j-1, res + "*")
    }
    @tailrec
    def printStarIter_i(i: Int, res: String): String = {
      if(i > n) res else printStarIter_i(i+1, res + printStarIter_j(i, "") + "\n")
    }
    printStarIter_i(1, "")
  }
  print(printStar(5))
}
{
  import scala.annotation.tailrec
  def abs(x: Int) = if(x < 0) -x else x
  def printStar(n: Int) = {
    @tailrec
    def printCharIter(j: Int, res: String, char: String): String = {
      if(j <= 0) res else printCharIter(j-1, res + char, char)
    }
    @tailrec
    def printStarIter(i: Int, res: String): String = {
      if(i <= -n) res
      else printStarIter(i-1, res + printCharIter(abs(i), "", " ") + printCharIter(2*(n-abs(i))-1, "", "+") + "\n")
    }
    printStarIter(n-1, "")
  }
  print(printStar(5))
}
{
  import scala.util.control.TailCalls._

  def sum1(n: Int, acc: Int): TailRec[Int] = {
    if(n <= 0) done(acc) else tailcall(sum2(n-1, n+acc))
  }
  def sum2(n: Int, acc: Int): TailRec[Int] = {
    if(n <= 0) done(acc) else tailcall(sum1(n-1, n+acc))
  }
  println(sum1(100000, 0).result)
}
{
  def fibo(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fibo(n-1) + fibo(n-2)
  }
  println(fibo(10))
}
{
  import scala.annotation.tailrec
  def fibo(n: Int): Int = {
    @tailrec
    def fiboTail(k: Int, fibo0: Int, fibo1: Int): Int = {
      if (k == n) fibo0
      else fiboTail(k+1, fibo0 + fibo1, fibo0)
    }
    fiboTail(0, 0, 1)
  }
  println(fibo(100))
}
{
  def ppa(p: (Int, Int)=>Int, a: Int, b: Int): Int = {
    if (a <= 0 || b <= 0) p(a, b)
    else if (p(a, b) % 2 == 0) p(a, ppa(p, a-1, b))
    else p(ppa(p, a, b-1), b)
  }
  println(ppa((a, b) => a + b, 100, 200))
}
{
  import scala.util.control.TailCalls._

  def ppa(p: (Int, Int)=>Int, a: Int, b: Int): Int = {
    def ppaCont(x: Int, y: Int, cont: Int=>TailRec[Int]): Int =
      if (x <= 0 || y <= 0) cont(p(x, y)).result
      else if (p(x, y) % 2 == 0) ppaCont(x-1, y, (r) => tailcall(cont(p(x, r))))
      else ppaCont(x, y-1, (r) => tailcall(cont(p(r, y))))
    ppaCont(a, b, (r) => done(r))
  }
  println(ppa((a, b) => a + b, 100, 200))
}
{
  def sum(n: Int): Int = {
    if (n <= 0) 0
    else n + sum(n-1)
  }
  println(sum(100))
}
{
  import scala.annotation.tailrec
  def sum(n: Int): Int = {
    @tailrec
    def sumTail(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else sumTail(n-1, n+acc)
    }
    sumTail(n, 0)
  }
  println(sum(1000000))
}
{
  import scala.annotation.tailrec
  import scala.util.control.TailCalls._

  def sum(n: Int): Int = {
    @tailrec
    def sumTail(n: Int, cont: Int=>TailRec[Int]): Int = {
      if (n <= 0) cont(0).result
      else sumTail(n-1, (r) => tailcall(cont(n + r)))
    }
    sumTail(n, (r) => done(r))
  }
  println(sum(1000000))
}
