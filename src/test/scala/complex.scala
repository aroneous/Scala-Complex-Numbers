package cc.wily.util

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

class StackSpec extends Spec with ShouldMatchers {
  val c3_5 = Complex(3,5)
  val c7_11 = Complex(7,11)
  val cm4_m6 = Complex(-4,-6)

  describe("Complex number operations") {

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
    }
  }
}

// vim: set ts=2 sw=2 et:
