package scalable.solutions

import scalaz.Id.Id

sealed trait Parity[F[_]] {
  def even[A](ns: List[A]): F[Boolean]

  def odd[A](ns: List[A]): F[Boolean]
}

object NotOptimized extends Parity[Id] {
  def even[A](ns: List[A]): Id[Boolean] =
    ns match {
      case Nil => true
      case x :: xs => odd(xs)
    }

  def odd[A](ns: List[A]): Id[Boolean] =
    ns match {
      case Nil => false
      case x :: xs => even(xs)
    }
}

object Optimized extends Parity[Trampoline] {
  def even[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(true)
      case x :: xs => More(() => odd(xs))
    }

  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(false)
      case x :: xs => More(() => even(xs))
    }
}

object ParityApp extends App {
  val n = 100000
  val ns = (1 to n).toList

  println("1. Trampolined:")
  val odd = Optimized.odd(ns).runT
  println(s" - odd: $odd")
  val even = Optimized.even(ns).runT
  println(s" - even: $even")

  println("2. Normal:")
  // both will raise StackOverflowError
  try {
    val odd1 = NotOptimized.odd(ns)
    println(s" - odd: $odd1")
  } catch {
    case e: Throwable => println(s" - odd: $e")
  }

  try {
    val even1 = NotOptimized.even(ns)
    println(s" - even: $even1")
  } catch {
    case e: Throwable => println(s" - even: $e")
  }
}
