import java.math.BigInteger
import java.util.Random

import org.scalatest.enablers.Length
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection
import scala.math.BigInt

class Lab3Test extends Specification {


  "The GF lab3" should {

//    "011 << 1 == 110" in new context {
//
//      val shift = 1
//      val expected = "110"
//
//      (A << shift).toString mustEqual expected
//    }
//
//    "011 << 2 == 100" in new context {
//      val shift = 2
//      val expected = "100"
//
//      (A << shift).toString mustEqual expected
//    }

    "A mod Gen == 100" in new context {
      val gen = "1101"
      1100110
      1011
       111110
       1011
        10010
        1011
          100
      val a = PB.fromString("1100110")

      a.toString mustEqual "11"
    }

    "Gen mod Gen == Zero" in new context {
      val a = PB.fromBinaryArray(PB.generatorBinaryVectorLittleEnd)

      a.toString mustEqual PB.Zero.toString
    }

    "A * One == A" in new context {
      val a = PB.fromString("1100110")

      (a mulPB PB.One).toString mustEqual a.toString
    }

    "A * Zero == Zero" in new context {
      val a = PB.fromString("1100110")

      (a mulPB PB.Zero).toString mustEqual PB.Zero.toString
    }

    "A * A == A.squared" in new context {

      val a = PB.fromString("1100111")

      (a mulPB a).toString mustEqual a.squaredPB().toString
    }

    "A * B == B * A" in new context {
      val a = PB.fromString("1100110")
      val b = PB.fromString("0101011")

      (a mulPB b).toString mustEqual (b mulPB a).toString
    }

    "C*(A+B) = C*A + C*B" in new context {
      val a = PB.fromString("1100110")
      val b = PB.fromString("0101010")
      val c = PB.fromString("0100011")

      (c mulPB(a plusPB b)).toString mustEqual ((c mulPB a) plusPB (c mulPB b)).toString
    }


    "A pow 1 = A" in new context {
      val a = PB.fromString("1100110")

      a.powPB(1).toString mustEqual (a).toString
    }


    " A pow 3 = A.squared * A" in new context {
      val a = PB.fromString("1100110")

      a.powPB(3).toString mustEqual (a mulPB a.squaredPB()).toString
    }

    " A.inverse == OK" in new context {
      val a = PB.fromString("101")

      a.inversePB.toString mustEqual "111"
    }

    " A.inverse.inverse == OK" in new context {
      val a = PB.fromString("101")

      (a.inversePB mulPB a).toString mustEqual PB.One.toString
    }


    "A.trace == OK" in new context {
      val a = PB.fromString("101")

      a.tracePB.toString mustEqual "0"
    }




    //    "convertors test" in new context {
//      val a = PB.fromString("1100110")
//
//      PB.fromPolynom(Seq(6, 5, 2, 1)).toString mustEqual a.toString
//    }

//    "A mod Gen == 110" in new context {
//      val generator = 1011
//      val actual = GF("1010") mod GF.generatorBigEndian
//
//      actual.toString mustEqual GF("1010") + GF(GF.generatorBigEndian)
//
//    }

  }

}

  // TODO: make generator not generate Zero or empty string
  // TODO: check A greater than B
  trait context extends Scope {

    val A = PB.fromString("011")
    val B = PB.fromString("110")
    val n = "11000001"
    println("A = " + A)
    println("B = " + B)
    val addCorrect =     "101"
    val mulCorrect =     "001"
    val squareCorrect =  "101"
    val inverseCorrect = "110"
    val powerCorrect =   "0010011"
    val add = A plusPB B
    val mul = A mulPB B
    val square = A.squaredPB()
//    val trace = A.trace
    val mod = PB.fromString("1010")
//    println("A mod Gen = " + mod)
//    println("Correct? :" + mod.toString.equals("110"))
//    println("A+B = " + add)
//    println("Correct? :" + add.toString.equals(addCorrect))
//    println("A*B = " + mul)
//    println("Correct? :" + mul.toString.equals(mulCorrect))
//    println("A^2 = " + square)
//    println("Correct? :" + square.toString.equals(squareCorrect))
//    val power = A ^ n
//    println("A^3 = " + power)
//    println("Correct? :" + power.toString.equals(powerCorrect))
//    println("trace = " + A.trace)
//    val inverse = A.inverse
//    println("inverse = " + inverse)
//    println("Correct? :" + inverse.toString.equals(inverseCorrect))
    println("one = " + PB.One)
    println("zero = " + PB.Zero)

  }
