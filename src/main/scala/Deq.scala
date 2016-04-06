package org.eold.coperque

import scala.collection.immutable.Iterable

class Deq {
  import Deq.Color._

  // TODO(hi-prio) implement
}

object Deq {

  object Color extends Enumeration {
    type Color = Value
    val Red, Yellow, Green = Value
  }

  /** Immutable buffer of size 0-5 for used by a non-catenanable deque */
  // Maintain invariant that sizes of stacks ft and bk differ by at most 1
  private[coperque] case class Buf[+A](val ft: Buf.Node[A], val bk: Buf.Node[A])
      extends DequeLike[A, Buf] with Deque[A] {

    def color = Math.abs(size) match {
      case 0 | 5 => Color.Red
      case 1 | 4 => Color.Yellow
      case _ => Color.Green
    }

    def size = {
      val (hasFront, hasBack) = (ft != null, bk != null)
      if (hasFront & hasBack) { // at least 2 elements
        val (hasInnerFront, hasInnerBack) = (ft.inner != null, bk.inner != null)
        if (hasInnerFront & hasInnerBack) { // 4 or 5 elements
          if (ft.inner.inner != null) -5 else if (bk.inner.inner != null) +5
          else 4
        } else { // 2 or 3 elements
          if (hasInnerFront) -3 else if (hasInnerBack) +3 else 2
        }
      } else { // 0 or 1 elements
        if (hasFront) -1 else if (hasBack) +1 else 0
      }
    }

    def isEmpty = ft == null && bk == null
    def nonEmpty = ft != null || bk != null

    override def push[B >: A](x: B): Buf[B] = {
      val (ft, bk) = Buf.cons(x, this.ft, this.bk)
      new Buf(ft, bk)
    }

    override def pop[B >: A]: (A, Buf[B]) = {
      val (value, (ft, bk)) = Buf.pop(this.ft, this.bk)
      (value, new Buf(ft, bk))
    }

    override def eject[B >: A]: (A, Buf[B]) = {
      val (value, (bk, ft)) = Buf.pop(this.bk, this.ft)
      (value, new Buf(ft, bk))
    }

    override def inject[B >: A](x: B): Buf[B] = {
      val (bk, ft) = Buf.cons(x, this.bk, this.ft)
      new Buf(ft, bk)
    }

    override def toString: String =
      s"${Option(ft).mkString(" ")}|${Option(bk).toList.reverse.mkString(" ")}"
  }

  private[coperque] object Buf {
    def apply[A]() = new Buf[A](null, null)
    def apply[A, B >: A](value1: A, value2: B) = new Buf[B](
      new Node(value1), new Node(value2))
    def apply[A](value1: A, value2: A, value3: A, value4: A) = new Buf[A](
      value1 ->: new Node(value2), value4 ->: new Node(value3))

    private[coperque]
    final case class Node[+A](val value: A, val inner: Node[_ <: A] = null)
        extends Iterable[A] { self =>
      def ->:[B >: A](outer: Node[B]): Node[B] = new Node(outer.value, this)
      def ->:[B >: A](value: B): Node[B] = new Node(value, this)
      def end = new Node(value)

      override def toString: String =
        if (value != null) value.toString else "null"
      override def iterator = new Iterator[A] {
        private var nd = self
        def hasNext = nd != null
        def next() = {
          val ret = nd.value
          nd = nd.inner
          ret
        }
      }
      // TODO(lo-prio): companion, newBuilder, ...
    }

    private def cons[A, B >: A](value: B, ft: Buf.Node[A], bk: Buf.Node[A]):
        (Buf.Node[B], Buf.Node[B]) = {
      val (hasFront, hasBack) = (ft != null, bk != null)
      if (hasFront & hasBack) {
        if (ft.inner != null) {
          if (bk.inner == null)
            return (value ->: ft.end, bk ->: ft.inner)
          else // TODO(lo-prio): optionally require via a macro
            require(ft.inner.inner == null && bk.inner.inner == null)
        }
        return (value ->: ft, bk)
      } else if (!hasBack)
        return (new Node(value, null), ft) // also ok if ft == null
      (new Node(value, null), bk)
    }

    private def pop[A, B >: A](ft: Buf.Node[A], bk: Buf.Node[A]):
        (A, (Buf.Node[B], Buf.Node[B])) = {
      val hasBack = bk != null
      if (hasBack && bk.inner != null) {
        if (ft.inner == null)
          return (ft.value, (bk.inner, bk.end))
        else if (bk.inner.inner != null)
          return (ft.value, (ft.inner ->: bk.inner.inner, bk ->: bk.inner.end))
      } else if (ft == null) {
        require(hasBack) // TODO(lo-prio): optionally require via a macro
        return (bk.value, (bk.inner, null))
      }
      (ft.value, (ft.inner, bk))
    }
  }

}
