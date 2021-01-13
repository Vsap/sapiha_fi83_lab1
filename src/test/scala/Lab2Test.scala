import java.math.BigInteger
import java.util.Random

import org.scalatest.enablers.Length
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection
import scala.math.BigInt

class Lab2Test extends Specification {

  import Lab2TestSupport._
  import Long16._
  import Long16Alg._

  import Conversions._


  "The 'Long16Alg' lab2" should {

    "isEven(Zero) == true == !isEven(One)" in new context {
      val evenZero = isEven(Zero)
      val evenOne = isEven(One)

      evenZero mustEqual true
      evenOne mustEqual false
    }

    "isEven(A) == BigInt(A).isEven" in new context {
      val evenA = isEven(a)
      val evenBigIntA = !(bigIntA.lowestSetBit == 0)

      //      evenBigIntA mustEqual false
      //      evenA mustEqual false
      evenA mustEqual evenBigIntA
    }

    "min(A,B) == BigInt(A).min(BigInt(B))" in new context {
      val bigIntMinAB: BigInt = bigIntA min bigIntB
      val minAB = Long16Alg.min(a, b)

      minAB.toHexString mustEqual bigIntMinAB.toHexString
    }

    "GCD(A,B) == BigInt(A).gcd(BigInt(B))" in new context {
      val bigIntGCD: BigInt = bigIntA gcd bigIntB
      val gcdAB = Long16Alg.gcd(b, a)

      gcdAB.toHexString mustEqual bigIntGCD.toHexString
    }

    "NSK(A,B) == BigInt(A).NSK(BigInt(B))" in new context {
      val bigIntGCD: BigInt = bigIntA gcd bigIntB
      val gcdAB = Long16Alg.nsk(a, b)

      gcdAB.toHexString mustEqual gcdAB.toHexString
    }
//
//    "getMu(N) == BigInt(N).calcMu " in new context {
//      val mu = getMu(n)
//      val bigIntMu = calcMu(bigIntN)
//
//      mu.toHexString mustEqual bigIntMu.toHexString
//    }


    "A mod N == BigInt(A) mod BigInt(N)" in new context {
      val bigIntMod: BigInt = bigIntA mod bigIntN
      val modA = Long16Alg.barrettReduction(a, n, getMu(n))

      modA.toHexString mustEqual bigIntMod.toHexString
    }

    "One mod N == One" in new context {
      //      val bigIntMod: BigInt = bigIntA mod bigIntN
      val modA = Long16Alg.barrettReduction(One, n, getMu(n))

      modA.toHexString mustEqual One.toHexString
    }
    "Zero mod N == Zero" in new context {
      //      val bigIntMod: BigInt = bigIntA mod bigIntN
      val modA = Long16Alg.barrettReduction(Zero, n, getMu(n))

      modA.toHexString mustEqual Zero.toHexString
    }

    "A + B mod N == BigInt(A) + BigInt(B) mod BigInt(N)" in new context {
      val bigIntSum: BigInt = (bigIntA + bigIntB) mod bigIntN
      val addedAB = Long16Alg.longAddMod(a, b, n)

      addedAB.toHexString mustEqual bigIntSum.toHexString
    }


    "A - B mod N == BigInt(A) - BigInt(B) mod BigInt(N)" in new context {
      val minusModAB = longSubMod(a, b, n)
      val bigMinusModAB = (bigIntA - bigIntB) mod bigIntN

      minusModAB.toHexString mustEqual bigMinusModAB.toHexString
    }

    "A * B mod N == BigInt(A) * BigInt(B) mod BigInt(N)" in new context {

      val longMul = longMulMod(a, b, n)
      val mu = getMu(n)

      val longMulModAB = barrettReduction(longMul, n, mu)
      val bigLongMulModAB = (bigIntA * bigIntB) mod bigIntN

      longMulModAB.toHexString mustEqual bigLongMulModAB.toHexString
    }

    "(A + B) * C mod N == C * A + C * B mod N" in new context {

      val sum1 = longAddMod(a, b, n)
      val mul1 = longMulMod(sum1, c, n)

      val longMul1 = longMulMod(a, c, n)
      val longMul2 = longMulMod(b, c, n)
      val sum2 = longAddMod(longMul1, longMul2, n)


      mul1.toHexString mustEqual sum2.toHexString
    }

    "A ^ 2 mod N == BigInt(A) ^ 2 mod BigInt(N)" in new context {
      val powerModA2 = longModPowerBarret(a, Two, n)
      val bigPowerModAB = (bigIntA pow 2) mod bigIntN

      powerModA2.toHexString mustEqual bigPowerModAB.toHexString
    }

    "A ^ 2 mod N == A * A mod N" in new context {
      val powerModA2 = longModPowerBarret(a, Two, n)
      val mulModA2 = barrettReduction(longMulMod(a, a, n), n, getMu(n))

      powerModA2.toHexString mustEqual mulModA2.toHexString
    }

    "A ^ 3 mod N == A * A * A mod N" in new context {
      val powerModA2 = longModPowerBarret(a, Three, n)
      val bigPowerModAB = barrettReduction(longMulMod(barrettReduction(longMulMod(a, a, n), n, getMu(n)), a, n), n , getMu(n))

      powerModA2.toHexString mustEqual bigPowerModAB.toHexString
    }

    "A ^ B mod N == BigInt(A)^BigInt(B) mod N" in new context {
      val powerModA2 = longModPowerBarret(a, b, n)
      val bigPowerModAB = bigIntA.modPow(bigIntB, bigIntN)

      powerModA2.toHexString mustEqual bigPowerModAB.toHexString
    }

  }

}

object Lab2TestSupport {

  def generateRandomSeq(length: Int = 6): Seq[Int] = {
    val randomizer = new scala.util.Random()

    for (
      i <- 0 until Long16.maxLength
    ) yield if (i < length) randomizer.nextInt(Long16.radix) else 0

  }
  def generateRandomHexString(length: Int = 6): String = {
    val randomizer = new scala.util.Random()

    (0 until length).map(i => Integer.toHexString(randomizer.nextInt(16))).mkString
  }
  def generateRandom(maxValue: Int): Int = new scala.util.Random().nextInt(maxValue + 1)

  implicit class TestStringOpt(val hex: String) extends AnyVal {
    def bigInt: BigInt = BigInt(hex, 16)
  }

  implicit class BigIntOpt(val bigInt: BigInt) extends AnyVal {
    def toHexString: String =
      bigInt.toString(16).takeRight(Long16.maxLength * 4).dropWhile(_ == "0")

    def toBinaryString: String =
      bigInt.toString(2).takeRight(Long16.maxLength * 4 * 4).dropWhile(_ == "0")
  }

  // TODO: make generator not generate Zero or empty string
  // TODO: check A greater than B
  trait context extends Scope {
    //    private val randomHexStringA = generateRandomHexString(128)
    //    val randomHexStringA = "D4D2110984907B5625309D956521BAB4157B8B1ECE04043249A3D379AC112E5B9AF44E721E148D88A942744CF56A06B92D28A0DB950FE4CED2B41A0BD38BCE7D" //generateRandomHexString(randomHexStringLengthA)
    //val randomHexStringA = "5EB19"
    val randomHexStringA = "D4D2110984907B5625309D956521BAB4157B8B1ECE04043249A3D379AC112E5B9AF44E721E148D88A942744CF56A06B92D28A0DB950FE4CED2B41A0BD38BCE7D0BE1055CF5DE38F2A588C2C9A79A75011058C320A7B661C6CE1C36C7D870758307E5D2CF07D9B6E8D529779B6B2910DD17B6766A7EFEE215A98CAC300F2827DB"
    val a = Conversions.fromHexString(randomHexStringA)


    //    private val randomHexStringB = generateRandomHexString(128)
    //    val randomHexStringB = "3A7EF2554E8940FA9B93B2A5E822CC7BB262F4A14159E4318CAE3ABF5AEB1022EC6D01DEFAB48B528868679D649B445A753684C13F6C3ADBAB059D635A288209" //generateRandomHexString(randomHexStringLengthB)
    //val randomHexStringB = "DE719" //generateRandomHexString(randomHexStringLengthB)
    val randomHexStringB = "3A7EF2554E8940FA9B93B2A5E822CC7BB262F4A14159E4318CAE3ABF5AEB1022EC6D01DEFAB48B528868679D649B445A753684C13F6C3ADBAB059D635A2882090FC166EA9F0AAACD16A062149E4A0952F7FAAB14A0E9D3CB0BE9200DBD3B0342496421826919148E617AF1DB66978B1FCD28F8408506B79979CCBCC7F7E5FDE7" //generateRandomHexString(randomHexStringLengthB)
    val b = Conversions.fromHexString(randomHexStringB)

    //    private val randomHexStringB = generateRandomHexString(128)
    //    val randomHexStringB = "3A7EF2554E8940FA9B93B2A5E822CC7BB262F4A14159E4318CAE3ABF5AEB1022EC6D01DEFAB48B528868679D649B445A753684C13F6C3ADBAB059D635A288209" //generateRandomHexString(randomHexStringLengthB)
    //val randomHexStringB = "DE719" //generateRandomHexString(randomHexStringLengthB)
    val randomHexStringC = "3A7EF2554E8940FA9B93B2A5E822CC7BB262F4A14159E4318CAE3ABF5AEB1022EC6D01DEFAB48B528868679D649B445A753684C13F6C3ADBAB059D635A2882090FC166EA9F0AAACD16A062149E4A0952F7FAAB14A0E9D3CB0BE9200DBD3B0342496421826919148E617AF1DB66978B1FCD28F8408506B79979CCBCC7F7E5FDE7" //generateRandomHexString(randomHexStringLengthB)
    val c = Conversions.fromHexString(randomHexStringC)


    //    private val randomHexStringMod = generateRandomHexString(128)
    //    val randomHexStringMod = "FAAE2DBD9EECEE161154B081A68CB675BFC633DF8811446F22C2AB317B4F76CFFC36AF3078C795EBB23EEB59DEA12EA2E2E7F05426B9FA209A9EF21DFBB4111A" //generateRandomHexString(randomHexStringLengthB)
    //val randomHexStringMod = "58319"
    val randomHexStringMod = "269D7722EA018F2AC35C5A3517AA06EAA1949059AE8240428BBFD0A8BE6E2EBF91223991F80D7413D6B2EB213E7122710EDEC617460FA0191F3901604619972018EBEF22D81AED9C56424014CADCC2CCDEE67D36A54BFC500230CA6693ABA057B374746622341ED6D52FE5A79E6860F54F197791B3FEF49FD534CB2C675B6BDB"
    val n = Conversions.fromHexString(randomHexStringMod)

    //    val bigIntA = shortRAndomA.bigInt
    //    val bigIntB = shortRandomHexStringB.bigInt

    val bigIntA = randomHexStringA.bigInt
    val bigIntB = randomHexStringB.bigInt
    val bigIntN = randomHexStringMod.bigInt


    def calcMu(n: BigInt) = {

      val k = n.bitLength
      val tempMu = BigInt(2).pow(2 * k)

      val mu = tempMu / n

      mu
    }


  }
}