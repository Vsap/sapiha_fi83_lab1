import java.math.BigInteger
import java.util.Random

import org.scalatest.enablers.Length
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection
import scala.math.BigInt

class Long16Test extends Specification {

  import Long16TestSupport._
  import Long16._
  import Conversions._


  "The 'Long16' lab1" should {

    "SomeFixedString == fromHexString().toHexString" in new context {
      val expectedFixedHexString = "cfb26dbd87c85a03434ef24c2e4c970d197df38ab03c66e9a3293fed1511b104faa03b31933959e0a6144dfb67ee9e10499aa67cd38a04f5dc64f20e5e84b6079c6f9ad7f97329f9784520b170b4f1d6c3ae19ebebedd12edff77c7bf4586643e69baeca8f0992eb8e2fda10f5724f3963668b41b19fac127c9acf894464d45cb1e29c8b8cfacb8ffac8c7c6e983602458dc0457206972b73a5b8d3b00a8e9995fb91aabc781b33e4ce6dd0573b53d9816aa861cd3bfda0ea7e6ab494908ddf28ea8776960721d404438c4cc406de966fd1dba4ca062aea00210b5b3795ad44cbaef67a576ccc3c3aa32092eaa91c0b4091778b0ce6077bd4c32e05e45350fb4"

      toHexString(fromHexString(expectedFixedHexString)) mustEqual expectedFixedHexString
    }

    "BigInt(HexString).toString == fromHexString().toHexString" in new context {
      val expectedHexStringFromRandom = bigIntA.toHexString
      val long16FromRandomHexStringToString = toHexString(fromHexString(randomHexStringA))


      long16FromRandomHexStringToString mustEqual expectedHexStringFromRandom
    }

    "A + B == BigInt(A) + BigInt(B)" in new context {
      val bigIntSum: BigInt = bigIntA + bigIntB
      val (addedAB, _) = Long16.longAdd(long16A, long16B)

      addedAB.toHexString mustEqual bigIntSum.toHexString
    }

    "A + B == B + A" in new context {
      val (addedAB, _) = Long16.longAdd(long16A, long16B)
      val (addedBA, _) = Long16.longAdd(long16B, long16A)

      addedAB mustEqual addedBA
    }

    // TODO: add support for negative numbers
    //    "A - B == BigInt(A) - BigInt(B)" in new context {
    //      val (minusAB, _) = Long16.longSub(long16A, long16B)
    //
    //      minusAB.toHexString mustEqual (bigIntA - bigIntB).toString(16)
    //    }

    "A - A == Zero" in new context {
      val (subAA, _) = Long16.longSub(long16A, long16A)

      subAA mustEqual Long16.Zero
    }

    "B + A - B == A" in new context {
      val (plusAminusB, _) = Long16.longSub(Long16.longAdd(long16B, long16A)._1, long16B)

      plusAminusB mustEqual long16A
    }

    "A digitShiftToHigh 1 == BigInt(A).shiftLeft(1 * W)" in new context {
      val (shifted, _) = Long16.longShiftDigitsToHigh(long16A)

      shifted.toHexString mustEqual (bigIntA << W).toHexString
    }

    "A digitShiftToHigh b == BigInt(A).shiftLeft(b * W)" in new context {
      val randomShift = generateRandom(32)
      val (shifted, _) = Long16.longShiftDigitsToHigh(long16A, randomShift)

      shifted.toHexString mustEqual (bigIntA << (randomShift * W)).toHexString
    }

    "A digitShiftToHigh maxLength == Zero" in new context {
      val randomShift = Long16.maxLength
      val (shifted, _) = Long16.longShiftDigitsToHigh(long16A, randomShift)

      shifted.toHexString mustEqual Zero.toHexString
    }


    "A * b == BigInt(A) * b" in new context {
      val b = generateRandom(Long16.digitMaxValue)
      val (mulAb, _) = Long16.longMulOneDigit(long16A, b)

      mulAb.toHexString mustEqual (bigIntA * BigInt(b)).toHexString
    }


    "A * Zero == Zero" in new context {
      val mulA0 = Long16.longMul(long16A, Zero)

      mulA0.toHexString mustEqual Long16.Zero.toHexString
    }

    "One * A == A" in new context {
      val mul1A = Long16.longMul(One, long16A)

      mul1A.toHexString mustEqual long16A.toHexString
    }

    "A * B == BigInt(A) * BigInt(B)" in new context {
      val mulAB = Long16.longMul(long16A, long16B)

      mulAB.toHexString mustEqual (bigIntA * bigIntB).toHexString
    }

    "A * B == B * A" in new context {
      val mulAB = Long16.longMul(long16A, long16B)
      val mulBA = Long16.longMul(long16B, long16A)

      mulAB.toHexString mustEqual mulBA.toHexString
    }

    "A shiftBits 0 == A" in new context {
      val randomBitsShift = 0
      val (long16AShifted, _) = Long16.longShiftBitsToHigh(long16A, randomBitsShift)

      long16AShifted.toHexString mustEqual long16A.toHexString
    }

    "One shiftBits b == BigInt(1) << b" in new context {
      val randomBitsShift = generateRandom(16)
      val (long16OneShifted, _) = Long16.longShiftBitsToHigh(One, randomBitsShift)

      long16OneShifted.toHexString mustEqual (BigInt(1) << randomBitsShift).toHexString
    }

    "A shiftBits b == BigInt(A) << b" in new context {
      val randomBitsShift = generateRandom(32)
      val (long16AShifted, _) = Long16.longShiftBitsToHigh(long16A, randomBitsShift)

      long16AShifted.toHexString mustEqual (bigIntA << randomBitsShift).toHexString
    }

    "A longCompr B == BigInt(A) compr BigInt(B)" in new context {

      Long16.longCmpr(long16A, long16B) mustEqual bigIntA.compare(bigIntB)
    }

    "bitLength(A) == BigInt(A).bitLength" in new context {

      Long16.longBitLength(long16A) mustEqual bigIntA.bitLength
      Long16.longBitLength(long16B) mustEqual bigIntB.bitLength
    }

    "A / A == One" in new context {
      val (long16Q, long16R) = Long16.longDivMod(long16A, long16A)

      long16Q.toHexString mustEqual One.toHexString
      long16R.toHexString mustEqual Zero.toHexString
    }

    "A / B == BigInt(A) / BigInt(B)" in new context {
      val AisGreaterThanOrEqualB = (bigIntA compare bigIntB) > -1
      val (long16Q, long16R) = if(AisGreaterThanOrEqualB)
        Long16.longDivMod(long16A, long16B)
      else Long16.longDivMod(long16B, long16A)
      val (bigIntQ, bigIntR) = if (AisGreaterThanOrEqualB) bigIntA /% bigIntB else bigIntB /% bigIntA

      long16Q.toHexString mustEqual bigIntQ.toHexString
      long16R.toHexString mustEqual bigIntR.toHexString
    }

    "A / B * B == A" in new context {

      val (long16Q, long16R) = Long16.longDivMod(long16A, long16B)
      val (result, _) = Long16.longAdd(long16R, Long16.longMul(long16Q, long16B))

      result.toHexString mustEqual long16A.toHexString
    }


    "toBitNum(A) == BigInt(A).toBinaryString" in new context {
      val binaryArr = Long16.toBinaryNumLittleEndian(long16A)
      val resultString = binaryArr.toBinaryString

      resultString mustEqual bigIntA.toBinaryString
    }

    "A pow 1 == A" in new context {
      val long16PowAOne = Long16.longPower1(long16A, One)


      long16PowAOne.toHexString mustEqual long16A.toHexString
    }

    "A pow 2 == A * A" in new context {
      val long16MulAA = Long16.longMul(long16A, long16A)
      val long16Two = fromHexString("2")
      val long16PowASquared = Long16.longPower1(long16A, long16Two)


      long16PowASquared.toHexString mustEqual long16MulAA.toHexString
    }

    "A pow 4 == (A * A) * (A * A)" in new context {
      val long16MulAA = Long16.longMul(long16A, long16A)
      val long16MulAAAA = Long16.longMul(long16MulAA, long16MulAA)
      val long16Two = fromHexString("4")
      val long16PowASquared = Long16.longPower1(long16A, long16Two)


      long16PowASquared.toHexString mustEqual long16MulAAAA.toHexString
    }

  }

}

object Long16TestSupport {

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
    private val randomHexStringLengthA = generateRandom(Long16.maxLength * 4)
    val randomHexStringA = "D4D2110984907B5625309D956521BAB4157B8B1ECE04043249A3D379AC112E5B9AF44E721E148D88A942744CF56A06B92D28A0DB950FE4CED2B41A0BD38BCE7D0BE1055CF5DE38F2A588C2C9A79A75011058C320A7B661C6CE1C36C7D870758307E5D2CF07D9B6E8D529779B6B2910DD17B6766A7EFEE215A98CAC300F2827DB" //generateRandomHexString(randomHexStringLengthA)
    val long16A = Conversions.fromHexString(randomHexStringA)


    private val randomHexStringLengthB = generateRandom(Long16.maxLength * 4)
    val randomHexStringB = "3A7EF2554E8940FA9B93B2A5E822CC7BB262F4A14159E4318CAE3ABF5AEB1022EC6D01DEFAB48B528868679D649B445A753684C13F6C3ADBAB059D635A2882090FC166EA9F0AAACD16A062149E4A0952F7FAAB14A0E9D3CB0BE9200DBD3B0342496421826919148E617AF1DB66978B1FCD28F8408506B79979CCBCC7F7E5FDE7" //generateRandomHexString(randomHexStringLengthB)
    val long16B = Conversions.fromHexString(randomHexStringB)

    val bigIntA = randomHexStringA.bigInt
    val bigIntB = randomHexStringB.bigInt


  }
}