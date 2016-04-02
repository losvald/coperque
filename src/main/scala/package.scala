package org.eold

package object coperque {

  import scala.language.higherKinds

  private[coperque] trait PushableLike[+A, +Repr[_] <: Pushable[_]] {
    self: Repr[_] =>
    def push[B >: A](x: B): Repr[B]
  }

  trait Pushable[+A] extends PushableLike[A, Pushable]

  private[coperque] trait PoppableLike[+A, +Repr[_] <: Poppable[_]] {
    self: Repr[_] =>
    def pop[B >: A]: Repr[B]
  }

  trait Poppable[+A] extends PoppableLike[A, Poppable]

  private[coperque] trait StackLike[+A, +Repr[_] <: Stack[_]]
      extends PushableLike[A, Repr] with PoppableLike[A, Repr] {
    self: Repr[_] =>
  }

  trait Stack[+A] extends StackLike[A, Stack]
      with Pushable[A] with Poppable[A]

  private[coperque] trait InjectableLike[+A, +Repr[_] <: Injectable[_]] {
    self: Repr[_] =>
    def inject[B >: A](x: B): Repr[B]
  }

  trait Injectable[+A] extends InjectableLike[A, Injectable]

  private[coperque] trait StequeLike[+A, +Repr[_] <: Steque[_]]
      extends StackLike[A, Repr]
      with InjectableLike[A, Repr] {
    self: Repr[_] =>
  }

  trait Steque[+A] extends StequeLike[A, Steque]
      with Stack[A] with Injectable[A]

  private[coperque] trait EjectableLike[+A, +Repr[_] <: Ejectable[_]] {
    self: Repr[_] =>
    def eject[B >: A]: Repr[B]
  }

  trait Ejectable[+A] extends EjectableLike[A, Ejectable]

  trait DequeLike[+A, +Repr[_] <: Deque[_]] extends StequeLike[A, Repr]
      with EjectableLike[A, Repr] {
    self: Repr[_] =>
  }

  trait Deque[+A] extends DequeLike[A, Deque] with Steque[A]
      with Ejectable[A]

  private[coperque] trait CatenableLike[+A, Repr[_] <: Catenable[_]] {
    self: Repr[_] =>
    def cat[B >: A](other: Repr[B]): Repr[B]
  }

  trait Catenable[+A]
}
