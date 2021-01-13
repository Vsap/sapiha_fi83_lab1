import java.math.BigInteger
import java.util.Random

import org.scalatest.enablers.Length
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection
import scala.math.BigInt
import NB._

class Lab4Test extends Specification {


  "The NB lab4" should {


    "A mod Gen == 101" in new context {
      val gen = "1011"
      1100110
      1011
      111110
      1011
      10010
      1011
      100
      val a = NB.fromNBString("101")

      a.toString mustEqual "101"
    }

//    "Gen mod Gen == Zero" in new context {
//      val a = NB.fromBinaryArray(NB.generatorBinaryVectorLittleEnd)
//
//      a.toString mustEqual NB.Zero.toString
//    }

    "A * One == A" in new context {
      val a = NB.fromNBString("101")

      (a mulNB NB.One).toString mustEqual a.toString
    }

    "A * Zero == Zero" in new context {
      val a = NB.fromNBString("1100110")

      (a mulNB NB.Zero).toString mustEqual NB.Zero.toString
    }

    "A * A == A.squared" in new context {
      1100110
      100
      10000
      1011
      110
      val a = NB.fromNBString("101")

      (a mulNB a).toString mustEqual a.squaredNB().toString
    }

    "A * B == B * A" in new context {
      val a = NB.fromNBString("101")
      val b = NB.fromNBString("011")

      (a mulNB b).toString mustEqual (b mulNB a).toString
    }

    "C*(A+B) = C*A + C*B" in new context {
      val a = NB.fromNBString("101")
      val b = NB.fromNBString("011")
      val c = NB.fromNBString("100")

      (c mulNB(a plusNB b)).toString mustEqual ((c mulNB a) plusNB (c mulNB b)).toString
    }

    " A.trace = Ok" in new context {
      val a = NB.fromNBString("101")
      val b = NB.fromNBString("100")


      a.traceNB mustEqual 0
      b.traceNB mustEqual 1
    }

    " A pow 1 = A" in new context {
      val a = NB.fromNBString("101")


      a.powNB(1).toString mustEqual (a).toString
    }


    " A pow 3 = A.squared * A" in new context {
      val a = NB.fromNBString("101")


      a.powNB(3).toString mustEqual (a mulNB a.squaredNB()).toString
    }

    " A.inverse == OK" in new context {
      val a = NB.fromNBString("101")


      a.inverseNB.toString mustEqual "1"
    }


   " A.inverse * A == One" in new context {
      val a = NB.fromNBString("101")


     (a.inverseNB mulNB a).toString mustEqual NB.One.toString
    }


    //    "convertors test" in new context {
//      val a = NB.fromNBString("1100110")
//
//      println(NB.MM)
//
//
//      NB.fromPolynom(Seq(6, 5, 2, 1)).toString mustEqual a.toString
//
//    }



    //    "A mod Gen == 110" in new context {
    //      val generator = 1011
    //      val actual = GF("1010") mod GF.generatorBigEndian
    //
    //      actual.toString mustEqual GF("1010") + GF(GF.generatorBigEndian)
    //
    //    }

  }

  trait context extends Scope {

    val A = NB.fromNBString("011")
    val B = NB.fromNBString("110")
    val n = "11000001"
    println("A = " + A)
    println("B = " + B)
    val addCorrect =     "101"
    val mulCorrect =     "001"
    val squareCorrect =  "101"
    val inverseCorrect = "110"
    val powerCorrect =   "0010011"
    val add = A plusNB B
    val mul = A mulNB B
    val square = A.squaredNB()
    //    val trace = A.trace
    val mod = NB.fromNBString("1010")
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
    println("one = " + NB.One)
    println("zero = " + NB.Zero)
    println(NB.MM.toList.map(_.toList))

  }
}

// TODO: make generator not generate Zero or empty string
// TODO: check A greater than B
