package com.easternsauce.game

import cats.Id
import cats.data.{State, StateT}

import java.util.concurrent.atomic.AtomicReference

sealed trait AtomicSTRef[A] {

  def aref: AtomicReference[A]

  def commit[B](s: State[A, B]): (A, B) = {
    val m = aref.get()
    val result = s.run(m).value
    if (!aref.compareAndSet(m, result._1))
      commit(s)
    else
      result
  }

}

object AtomicSTRef {

  def get[A]: StateT[Id, A, A] = StateT[Id, A, A](a => (a, a))

  def update[A](na: A): StateT[Id, A, List[ExternalEvent]] = StateT[Id, A, List[ExternalEvent]](a => (na, List()))

  def apply[A](a: A): AtomicSTRef[A] =
    new AtomicSTRef[A] {
      private val ref = new AtomicReference(a)
      def aref: AtomicReference[A] = ref
    }
}
