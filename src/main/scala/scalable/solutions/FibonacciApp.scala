package scalable.solutions

import scala.annotation.tailrec

object FibonacciApp extends App {

  def fibonacci(n: Int): Int = {

    def fib(n: Int, a: Int, b: Int): Trampoline[Int] = n match {
      case 0 => Done(a)
      case _ => More(() => fib(n - 1, b, a + b))
    }

    assert(n >= 0)
    fib(n, 0, 1).runT
  }

  def fib2(n: Int): Int = {

    @tailrec
    def fib(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fib(n - 1, b, a + b)
    }

    assert(n >= 0)
    fib(n, 0, 1)
  }

  def fib1(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fib1(n - 1) + fib1(n - 2)
  }

  val n = 32
  println(fibonacci(n))
  println(fib2(n))
  println(fib1(n))
}
