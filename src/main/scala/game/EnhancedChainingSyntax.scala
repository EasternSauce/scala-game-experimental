package game

import scala.language.implicitConversions

object EnhancedChainingSyntax {
  @`inline` implicit final def enhancedScalaUtilChainingOps[A](a: A): EnhancedChainingOps[A] =
    new EnhancedChainingOps(a)
}

final class EnhancedChainingOps[A](private val self: A) extends AnyVal {

  def tap[U](f: A => U): A = {
    f(self)
    self
  }

  def pipe(f: A => A): A = f(self)

  def pipeIf(c: Boolean)(f: A => A): A = if (c) f(self) else self
}
