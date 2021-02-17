package scalable.solutions

import scala.annotation.tailrec

sealed trait Trampoline[+A] {
  @tailrec
  final def runT: A =
    this match {
      case More(k) => k().runT
      case Done(v) => v
    }
}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

case class Done[+A](result: A) extends Trampoline[A]
