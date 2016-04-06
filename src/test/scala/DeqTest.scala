package org.eold.coperque

import org.scalatest.{FunSpec, GivenWhenThen}

import Deq._
import Color._

class BufSpec extends FunSpec with GivenWhenThen {
  describe("An empty Buf") {
    val b0 = Buf[Int]()
    it("must have size 0 and be red") {
      assert(b0.size === 0)
      assert(b0.color === Red)
    }

    describe("When an element is pushed") {
      val v1 = -10
      lazy val b1 = b0.push(v1)
      it("must have size 1 and be yellow") {
        assert(b1.size == -1)
      }

      describe("When an element is injected") {
        val v2 = 20
        lazy val b2 = b1.inject(v2)
        it("must have size 2 and be green") {
          assert(b2.size == 2)
          assert(b2.color == Green)
        }

        it("should have same front") {
          assert(b2.ft == b1.ft)
        }

        it("should be same as inject followed by push on an empty") {
          assert(b2 == b0.inject(20).push(-10))
        }

        describe("When ejected") {
          lazy val (v2e, b2e) = b2.eject
          it("should be the same as before either") { assert(b2e == b1) }
          it("must return correct value") { assert(v2e == v2) }
        }
      }

      describe("When popped or ejected") {
        it("must be empty again and return correct value") {
          val (v1o, b1o) = b1.pop
          assert(b1o == b0)
          assert(v1o == v1)
          val (v1e, b1e) = b1.eject
          assert(b1e == b0)
          assert(v1o == v1)
        }
      }
    }

    describe("When an element is injected") {
      val v1 = 10
      lazy val b1 = b0.inject(v1)
      it("must have size 1 and be yellow") {
        assert(b1.size == 1)
      }

      describe("When an element is pushed") {
        val v2 = -20
        lazy val b2 = b1.push(v2)
        it("must have size 2 and be green") {
          assert(b2.size == 2)
          assert(b2.color == Green)
        }

        it("should have same back") {
          assert(b2.bk == b1.bk)
        }

        it("should be same as push followed by inject on an empty") {
          assert(b2 == b0.push(-20).inject(10))
        }

        describe("When popped") {
          lazy val (v2o, b2o) = b2.pop
          it("should be the same as before either") { assert(b2o == b1) }
          it("must return correct value") { assert(v2o == v2) }
        }
      }

      describe("When ejected or popped") {
        it("must be empty again and return correct value") {
          val (v1e, b1e) = b1.eject
          assert(b1e == b0)
          assert(v1e == v1)
          val (v1o, b1o) = b1.pop
          assert(b1o == b0)
          assert(v1o == v1)
        }
      }
    }

    it("should throw an IllegalArgumentException if an element is popped") {
      intercept[IllegalArgumentException] { b0.pop }
    }
    it("should throw an IllegalArgumentException if an element is ejected") {
      intercept[IllegalArgumentException] { b0.eject }
    }
  }

  describe("A Buf of size 2") {
    lazy val b2 = Buf(-15, 15)

    describe("When an element is pushed") {
      lazy val b3 = b2.push(-30)
      it("must have size 3 and still be green") {
        assert(b3.size == -3)
        assert(b3.color == Green)
      }
      it("should have that element added to front") {
        assert(b3.ft.value == -30)
        assert(b3.ft.inner == b2.ft)
      }

      describe("When an element is pushed") {
        lazy val b4 = b3.push(-40)
        it("must have size 4 and be yellow") {
          assert(b4.size == 4)
          assert(b4.color == Yellow)
        }

        it("should have the correct 2 elements at both front and back") {
          assert(b4.ft.size == b4.bk.size)
          assert(b4.ft.toList == List(-40, -30))
          assert(b4.bk.toList == List(+15, -15))
        }
      }

      describe("When an element is injected") {
        lazy val b4 = b3.inject(40)
        it("must have size 4 and be yellow") {
          assert(b4.size == 4)
          assert(b4.color == Yellow)
        }

        it("should have the correct 2 elements at both front and back") {
          assert(b4.ft.size == b4.bk.size)
          assert(b4.ft.toList == List(-30, -15))
          assert(b4.bk.toList == List(+40, +15))
        }
      }

      describe("When popped or ejected") {
        it("must have correct elements and return correct value") {
          val (v3o, b3o) = b3.pop
          assert(v3o == -30)
          assert(b3o.ft.toList == List(-15))
          assert(b3o.bk.toList == List(+15))
          val (v3e, b3e) = b3.eject
          assert(v3e == +15)
          assert(b3e.ft.toList == List(-30))
          assert(b3e.bk.toList == List(-15))
        }
      }
    }

    describe("When an element is injected") {
      lazy val b3 = b2.inject(30)
      it("must have size 3 and still be green") {
        assert(b3.size == +3)
        assert(b3.color == Green)
      }
      it("should have that element added to back") {
        assert(b3.bk.value == 30)
        assert(b3.bk.inner == b2.bk)
      }

      describe("When an element is injected") {
        lazy val b4 = b3.inject(40)
        it("must have size 4 and be yellow") {
          assert(b4.size == 4)
          assert(b4.color == Yellow)
        }

        it("should have the correct 2 elements at both front and back") {
          assert(b4.ft.size == b4.bk.size)
          assert(b4.ft.toList == List(-15, 15))
          assert(b4.bk.toList.reverse == List(30, 40))
        }
      }

      describe("When an element is pushed") {
        lazy val b4 = b3.push(-40)
        it("must have size 4 and be yellow") {
          assert(b4.size == 4)
          assert(b4.color == Yellow)
        }

        it("should have the correct 2 elements at both front and back") {
          assert(b4.ft.size == b4.bk.size)
          assert(b4.ft.toList == List(-40, -15))
          assert(b4.bk.toList == List(+30, +15))
        }
      }

      describe("When ejected or popped") {
        it("must have correct elements and return correct value") {
          val (v3e, b3e) = b3.eject
          assert(v3e == +30)
          assert(b3e.ft.toList == List(-15))
          assert(b3e.bk.toList == List(+15))
          val (v3o, b3o) = b3.pop
          assert(v3o == -15)
          assert(b3o.ft.toList == List(+15))
          assert(b3o.bk.toList == List(+30))
        }
      }
    }
  }

  describe("A Buf of size 4") {
    val b4 = Buf(-35, -15, +15, +35)

    describe("When popped") {
      lazy val (v4, b4o) = b4.pop
      it("must be green") { assert(b4o.color == Green) }
      it("must return correct value") { assert(v4 == -35) }
      it("should have correct 2 (1) elements at front (back)") {
        assert(b4o.ft.toList == List(-15))
        assert(b4o.bk.toList == List(+35, +15))
      }
    }

    describe("When ejected") {
      lazy val (v4, b4e) = b4.eject
      it("must be green") { assert(b4e.color == Green) }
      it("must return correct value") { assert(v4 == +35) }
      it("should have correct 1 (2) elements at back (front)") {
        assert(b4e.ft.toList == List(-35, -15))
        assert(b4e.bk.toList == List(+15))
      }
    }

    describe("When an element is pushed") {
      lazy val b5 = b4.push(-55)
      it("must have size 5 and be red") {
        assert(b5.size == -5)
        assert(b5.color == Red)
      }
      it("should have correct 3 (2) elements at front (back)") {
        assert(b5.ft.toList == List(-55, -35, -15))
        assert(b5.bk.toList == List(+35, +15))
      }

      describe("When popped or ejected") {
        it("must have correct elements and return correct value") {
          val (v5o, b5o) = b5.pop
          assert(v5o == -55)
          assert(b5o.ft.toList == List(-35, -15))
          assert(b5o.bk.toList == List(+35, +15))
          val (v5e, b5e) = b5.eject
          assert(v5e == +35)
          assert(b5e.ft.toList == List(-55, -35))
          assert(b5e.bk.toList == List(+15, -15))
        }
      }

      describe("When an element is pushed") {
        it("should throw an IllegalArgumentException") {
          intercept[IllegalArgumentException] { b5.push(-666) }
        }
      }
      describe("When an element is injected") {
        it("should throw an IllegalArgumentException") {
          intercept[IllegalArgumentException] { b5.inject(+666) }
        }
      }
    }

    describe("When an element is injected") {
      lazy val b5 = b4.inject(+55)
      it("must have size 5 and be red") {
        assert(b5.size == 5)
        assert(b5.color == Red)
      }
      it("should have correct 3 (2) elements at back (front)") {
        assert(b5.ft.toList == List(-35, -15))
        assert(b5.bk.toList == List(+55, +35, +15))
      }

      describe("When ejected or popped") {
        it("must have correct elements and return correct value") {
          val (v5e, b5e) = b5.eject
          assert(v5e == +55)
          assert(b5e.ft.toList == List(-35, -15))
          assert(b5e.bk.toList == List(+35, +15))
          val (v5o, b5o) = b5.pop
          assert(v5o == -35)
          assert(b5o.ft.toList == List(-15, +15))
          assert(b5o.bk.toList == List(+55, +35))
        }
      }

      describe("When an element is pushed") {
        it("should throw an IllegalArgumentException") {
          intercept[IllegalArgumentException] { b5.push(-666) }
        }
      }
      describe("When an element is ejected") {
        it("should throw an IllegalArgumentException") {
          intercept[IllegalArgumentException] { b5.inject(666) }
        }
      }
    }
  }
}
