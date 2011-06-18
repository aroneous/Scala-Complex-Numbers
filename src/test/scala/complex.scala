package cc.wily.util

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

class StackSpec extends Spec with ShouldMatchers {
  val c3_5 = Complex(3,5)
  val c3_5_2 = Complex(3,5)
  val c7_11 = Complex(7,11)
  val cm4_m6 = Complex(-4,-6)
  // Following pair are equivalent, defined in two coordinate systems.
  val c1_0r = RectComplex(1,0)
  val c1_0p = PolarComplex(1, 0)
  val c0_0 = Complex(0,0)

  // A trivial subclass that says it can't equal a superclass
  class NotEqualComplex(r:Double, i:Double) extends RectComplex(r, i) {
    override def equals(other:Any) = {
      if (other.isInstanceOf[NotEqualComplex]) {
        super.equals(other)
      } else {
        false
      }
    }

    override def canEqual(other:Any) = {
      other.isInstanceOf[NotEqualComplex]
    }
  }
  val c3_5NE = new NotEqualComplex(3,5)

  val myDouble = 11.0d

  describe("Complex number operations") {
    describe("equality") {
      it("should equal itself") {
        (c3_5) should equal (c3_5)
        (c1_0p) should equal (c1_0p)
      }
      it("should equal an identical complex") {
        (c3_5) should equal (c3_5_2)
        (c3_5_2) should equal (c3_5)
      }
      it("should equal an identical complex expressed in other coordinates") {
        (c1_0r) should equal (c1_0p)
        (c1_0p) should equal (c1_0r)
      }
      it("should have identical hash code as equal objects") {
        (c3_5.hashCode) should equal (c3_5_2.hashCode)
        (c1_0r.hashCode) should equal (c1_0p.hashCode)
      }
      it("should equal an identical subclass of itself that doesn't redefine equals") {
        val anon = new RectComplex(9,5) {
          override val real = 3.0d
        }
        (anon) should equal (c3_5)
      }
      it("should not equal a different complex") {
        (c3_5) should not equal (c7_11)
      }
      it("should not equal a subclass that says it can't equal Complex") {
        (c3_5) should not equal (c3_5NE)
        (c3_5NE) should not equal (c3_5)
      }
    }

    describe("addition") {

      it("should add the respective components") {
        val csum = c3_5 + c7_11
        csum.real should equal (c3_5.real + c7_11.real)
        csum.imag should equal (c3_5.imag + c7_11.imag)
      }

      it("should commute") {
        (c3_5 + c7_11) should equal (c7_11 + c3_5)
      }

      it("should handle negative components") {
        val csum = c3_5 + cm4_m6
        csum.real should equal (c3_5.real + cm4_m6.real)
        csum.imag should equal (c3_5.imag + cm4_m6.imag)
      }

      it("should sum with a double") {
        val csum = c3_5 + myDouble
        (csum.real) should equal (c3_5.real + myDouble)
        (csum.imag) should equal (c3_5.imag)
      }

      it("should sum with a double that comes first") {
        val csum = myDouble + c3_5
        (csum.real) should equal (c3_5.real + myDouble)
        (csum.imag) should equal (c3_5.imag)
      }
    }

    describe("subtract") {

      it("should subtract the respective components") {
        val cdiff = c3_5 - c7_11
        cdiff.real should equal (c3_5.real - c7_11.real)
        cdiff.imag should equal (c3_5.imag - c7_11.imag)
      }

      it("should handle negative components") {
        val cdiff = c3_5 - cm4_m6
        cdiff.real should equal (c3_5.real - cm4_m6.real)
        cdiff.imag should equal (c3_5.imag - cm4_m6.imag)
      }

      it("should sum with a double") {
        val cdiff = c3_5 - myDouble
        (cdiff.real) should equal (c3_5.real - myDouble)
        (cdiff.imag) should equal (c3_5.imag)
      }

      it("should sum with a double that comes first") {
        val cdiff = myDouble - c3_5
        (cdiff.real) should equal (myDouble - c3_5.real)
        (cdiff.imag) should equal (-(c3_5.imag))
      }
    }

    describe("negation") {
      it("should invert the polarity of the rectangular components") {
        ((-c3_5).real) should equal (-(c3_5.real))
        ((-c3_5).imag) should equal (-(c3_5.imag))
      }
      it("should not change (0,0)") {
        (c0_0) should equal (-c0_0)
      }
    }

    describe("conjugation") {
      it("should invert the polarity of the imaginary component") {
        ((c3_5*).real) should equal (c3_5.real)
        ((c3_5*).imag) should equal (-(c3_5.imag))
      }
      it("should not change (0,0)") {
        (c0_0) should equal (c0_0*)
      }
      it("should work the same for both forms of the call") {
        (c3_5 conjugate) should equal (c3_5*)
      }
    }
  }
}

// vim: set ts=2 sw=2 et:
