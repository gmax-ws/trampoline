package scalable.solutions

import scala.annotation.tailrec

object FactorialApp extends App {

  def factorial(n: Int, product: BigInt): Trampoline[BigInt] =
    if (n < 2) Done(product) else More(() => factorial(n - 1, n * product))

  @tailrec
  def fact(n: Int, product: BigInt): BigInt =
    if (n < 2) product else fact(n - 1, n * product)

  // an interesting factorial calculation
  def factorial(n: BigInt): BigInt =
    (BigInt(1) to n).product

  def factorial2(n: BigInt): BigInt =
    (BigInt(1) to n).foldLeft(BigInt(1))((f, v) => f * v)

  val n = 1000
  val f1 = factorial(n, 1).runT
  val f2 = fact(n, 1)
  val f3 = factorial(n)
  val f4 = factorial2(n)

  println(f1)
  println(f2)
  println(f3)
  println(f4)

  assert(f1 == f2 && f1 == f3 && f1 == f4)
}
