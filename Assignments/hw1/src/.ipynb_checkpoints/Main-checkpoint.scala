package pp202002.hw1
import scala.annotation.tailrec
import scala.util.control.TailCalls._

object Main {
  /* Implement given functions, which is currently blank. (???) */

  /*
    **WARNING: Please read the restrictions below carefully.**

    If you do not follow it, **your submission will not be graded.**

    1. Do not use keyword `var`. Use `val` and `def` instead.
    2. Do not use any library functions or data structures like `List`, `Array` `Range` (`1 to n`, `1 until n` ...), 
       `fold`, `map`, `reduce` or etc... You can only use tuples, `scala.annotation.tailrec`, and 
       `scala.util.control.TailCalls._` from the library.

    Again, if you do not follow these rules, your score will be zero.

    For all three problems, 50% of the test cases will require tail call optimizations (ie, have large inputs) 
    and the other 50% will not (ie, have small inputs).

    So, we will get 50% of the score if you submit a correct program without tail call optimization.
   */

  /*
   Exercise 1: Collatz sequence

   Consider this function: for a positive integer n,
   f(n) = n / 2       (if n is even)
   f(n) = 3 * n + 1   (if n is odd)

   Collatz sequence is defined as follows:
   c_0 = n  (n is positive integer)
   c_k = f(c_(k-1))

   Collatz conjecture(https://en.wikipedia.org/wiki/Collatz_conjecture) is the famous open problem,
   that no matter what value of starting value 'n', the sequence will always reach 1.

   Given a number n (1 <= n <= 10^12), compute the length and the sum of collatz sequence
   starting with n, until it reaches 1.

   e.g.) collatz(5) == (6, 36)
         (5 -> 16 -> 8 -> 4 -> 2 -> 1)
   */
    
//   def collatz(n: Long): (Long, Long) = {
//     if (n == 1) (1, 1)
//     else {
//       val res = if (n % 2 == 0) collatz(n/2) else collatz(3*n + 1)
//       (res._1 + 1, res._2 + n)
//     }
//   }

  def collatz(n: Long): (Long, Long) = {
    @tailrec
    def collatzTail(n: Long, res: (Long, Long)): (Long, Long) = {
        if (n == 1) (res._1 + 1, res._2 + 1)
        else if (n % 2 == 0) collatzTail(n/2, (res._1 + 1, res._2 + n))
        else collatzTail(3*n+1, (res._1 + 1, res._2 + n))
    }
    collatzTail(n, (0, 0))
  }

  /*
   Exercise 2: Definite integral by Riemann sum

   Implement definite integral:
   Given a continuous function f, and range [a, b], compute the definite integral value over the interval [a, b]

   The value should be calculated by Riemann sum. (https://terms.naver.com/entry.nhn?docId=2073839&cid=47324&categoryId=47324)
   Divide the interval [a, b] into n equal intervals, and gather the areas below function f through each interval.

   The difference between your result and the real integral value should be less than 0.001

   Hint: Increase the number of subdivision `n` until the value moves less then 0.001
   */
  def integral(f: Double => Double, a: Double, b: Double): Double = {
    @tailrec
    def integralTail(n: Double, res: Double): Double = {
      @tailrec
      def RiemannTail(delta: Double, x: Double, sum: Double): Double = {
        if (x >= b) sum
        else RiemannTail(delta, x + delta, sum + f(x)*delta)
      }
      val sum = RiemannTail((b-a)/n, a, 0)
      if (math.abs(res - sum) < 0.001) return sum
      else integralTail(n*2, sum)
    }

    integralTail(128, 0)
  }

  /*
   Exercise 3: Ping-pong Ackermann function

   Given a function p and positive integer a and b, implement this function `ppa`: (0 <= n <= 10^6)
   ppa(p, a, b) = p(a, b)               (if a <= 0 or b <= 0)
   ppa(p, a, b) = p(a, ppa(p, a-1, b))  (if p(a, b) is even and a, b > 0)
   ppa(p, a, b) = p(ppa(p, a, b-1), b)  (if p(a, b) is odd and a, b > 0)


   Hint 1: If n > 0, wrap remained calculations, make it as an anonymous function and pass it to the parameter recursively.
           That anonymous function is called 'Continuation'. You'd better search 'Continuation Passing Style'.

   Hint 2: Due to the limits of Scala's platform (JVM), tail call (not tail recursion) is not optimized generally,
           especially when using continuation passing style.
           To optimize tail call properly, use `tailcall`, `done`, `result` functions from scala.util.control.TailCalls._
           (See https://stackoverflow.com/questions/16539488/why-scala-doesnt-make-tail-call-optimization)
   */
    
//   def ppa(p: (Int, Int)=>Int, a: Int, b: Int): Int = {
//     def ppaCont(a: Int, b: Int, cont: (Int, Int) => Int): Int = {
//       if (a <= 0 || b <= 0) cont(a, b)
//       else if (p(a, b) % 2 == 0) ppaCont(a-1, b, (x: Int, y: Int) => cont(a, p(x, y)))
//       else ppaCont(a, b-1, (x: Int, y: Int) => cont(p(x, y), b))
//     }
//     ppaCont(a, b, p)
//   }
    
  def ppa(p: (Int, Int)=>Int, a: Int, b: Int): Int = {
    def ppaCont(a: Int, b: Int, cont: (Int, Int) => TailRec[Int]): TailRec[Int] = {
      if (a <= 0 || b <= 0) cont(a, b)
      else if (p(a, b) % 2 == 0) ppaCont(a-1, b, (x: Int, y: Int) => tailcall(cont(a, p(x, y))))
      else ppaCont(a, b-1, (x: Int, y: Int) => tailcall(cont(p(x, y), b)))
    }
    ppaCont(a, b, (x: Int, y: Int) => done(p(x, y))).result
  }
}