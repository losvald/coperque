package org.eold.coperque

object StaticTest {
  trait MyStack[+A] extends Stack[A] {
    def refinedReturnType[B >: A](x: B): Stack[B] =
      push(x).pop
  }

  trait MyDeque[+A] extends Deque[A]
      with DequeLike[A, MyDeque]  {
    def refinedReturnType[B >: A](x: B): MyDeque[B] =
      eject.inject(x)
  }

  def testPushCovariant(stk: MyStack[String]) {
    val stkSt: Stack[String] = stk.push("foo")
    val stkAny: Stack[Any] = stk.push(42)
  }

  def testEjectCovariant(deq: MyDeque[String]) {
    val deqStr: MyDeque[String] = deq.eject
    val deqAny: MyDeque[Any] = deq.eject
  }

  case class CatStr[+A](lst: List[A]) extends CatenableLike[A, CatStr]
      with Catenable[A] {
    override def cat[B >: A](other: CatStr[B]) = CatStr(this.lst ::: other.lst)
  }
}
