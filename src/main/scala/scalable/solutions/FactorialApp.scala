package scalable.solutions

import scala.annotation.tailrec

object FactorialApp extends App {

  def factorial(n: Int, product: BigInt): Trampoline[BigInt] = {
    if (n <= 2) Done(product) else More(() => factorial(n - 1, n * product))
  }

  @tailrec
  def fact(n: Int, product: BigInt): BigInt = {
    if (n <= 2) product else fact(n - 1, n * product)
  }

  val n = 100000
  println(factorial(n, 1).runT)
  println(fact(n, 1))
}
