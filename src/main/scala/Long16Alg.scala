import Long16._
import scala.annotation.tailrec


object Long16Alg {
  type LongNumber = Seq[Int]

  def isEven(thisArray: LongNumber): Boolean = (thisArray.head & 1) == 0

  def min(a: LongNumber, b: LongNumber): LongNumber =
    if (longCmpr(a, b) == 1) b
    else a

  def longSubAbs(a: LongNumber, b: LongNumber): LongNumber =
    if (longCmpr(a, b) == 1) longSub(a, b)._1
    else longSub(b, a)._1


  //  d := 1;
  //while (a – парне) and (b парне) do: // виокремлення загальної парної частини
  //a := a / 2;
  //b := b / 2;
  //d := d * 2;
  //while (a – парне) do:
  //a := a / 2;
  //while (b <> 0) do:
  //while (b – парне) do:
  //b := b / 2;
  //(a, b) := (min{a, b}, abs(a – b))
  //d := d * a;
  //return d;
  def gcd(a: LongNumber, b: LongNumber): LongNumber = {

    @tailrec
    def gcdAcc(a: LongNumber, b: LongNumber, result: LongNumber = One): LongNumber =
      if (longCmpr(b, Zero) == 1) {
        if (isEven(a) && isEven(b))
          gcdAcc(longShiftBitToLow(a), longShiftBitToLow(b), longShiftBitsToHigh(result, 1)._1)
        else if (isEven(a) && !isEven(b))
          gcdAcc(longShiftBitToLow(a), b, result)
        else if (!isEven(a) && isEven(b))
          gcdAcc(a, longShiftBitToLow(b), result)
        else gcdAcc(min(a, b), longSubAbs(a, b), result)
      } else longMul(a, result)

    def gcdImper(a: LongNumber, b: LongNumber): LongNumber = {
      var tempA = a
      var tempB = b
      var d = One

      while (isEven(tempA) && isEven(tempB)) {
        tempA = longShiftBitToLow(tempA)
        tempB = longShiftBitToLow(tempB)
        d = longShiftBitsToHigh(d, 1)._1
      }

      while (isEven(tempA))
        tempA = longShiftBitToLow(tempA)

      while (longCmpr(tempB, One) != 0) {
        while (isEven(tempB))
          tempB = longShiftBitToLow(tempB)

        val tempA2 = tempA
        val tempB2 = tempB

        tempA = min(tempA2, tempB2)
        tempB = longSubAbs(tempA2, tempB2)
      }

      d = longMul(d, tempA)

      d
    }

    def gcdHabr(a: LongNumber, b: LongNumber): LongNumber = {

      var tempA = a
      var tempB = b
      var d = One

      while (isEven(tempA) && isEven(tempB)) {
        tempA = longShiftBitToLow(tempA)
        tempB = longShiftBitToLow(tempB)
        d = longShiftBitsToHigh(d, 1)._1
      }

      while (isEven(tempA))
        tempA = longShiftBitToLow(tempA)

      while (isEven(tempB))
        tempB = longShiftBitToLow(tempB)


      while (longCmpr(tempA, tempB) != 0) {
        if (longCmpr(tempA, tempB) == 1) {
          val tmp = tempA
          tempA = tempB
          tempB = tmp
        }

        tempB = longSub(tempB, tempA)._1
      }

      longMul(tempA, d)
    }

    //    gcdAcc(a, b)

    //    gcdImper(a, b)

    if (longCmpr(a, Zero) == 0 || longCmpr(b, Zero) == 0) longAdd(a, b)._1
    else gcdHabr(a, b)
  }


  def nsk(a: LongNumber, b: LongNumber): LongNumber = {
    longDivMod(longMul(a, b), gcd(a, b))._1
  }

  def killLastDigits(a: LongNumber, amountOfDigits: Int): LongNumber = {
    val (result, _) = longShiftDigitsToLow(a, amountOfDigits)
    result
  }

  def killLastBits(a: LongNumber, amountOfBits: Int): LongNumber = {
    longShiftBitsToLow(a, amountOfBits)
  }

  def getMu(n: LongNumber): LongNumber = {
    val k = Long16.longDigitLength(n)
    val (tempMu, carryMu) = longShiftDigitsToHigh(One, 2 * k)
    val (mu, rMu) = longDivMod(tempMu, n)

    mu
  }

  //  q := KillLastDigits(x, k-1); // відкидання останніх k-1 цифри
  //q := q * ;
  //q := KillLastDigits(q, k+1);
  //r := x – q * n;
  //while (r >= n) do: // Барретт гарантує, що цикл виконується
  //r := r – n; // не більше двох разів
  //return r;
  def barrettReduction(x: LongNumber, n: LongNumber, mu: LongNumber): LongNumber = {
    val cmpr = longCmpr(x, n)
    if (cmpr == 1) {
      val k = longDigitLength(n)
      val q = killLastDigits(x, k - 1)
      val q1 = longMul(q, mu)
      val q2 = killLastDigits(q1, k + 1)
      var r = longSub(x, longMul(q2, n))._1
      var i = 1
      while (longCmpr(r, n) == 1) {
        //        println("Reduction i = " + i + "R = " + Conversions.toHexString(r))
        i = i + 1
        r = longSub(r, n)._1
      }

      r
    }
    else if (cmpr == -1) x
    else Zero
  }


  //C := 1;
  // := LongShiftDigitsToHigh(1, 2*k) / n; // єдине ділення!
  //for i := 0 to m-1 do:
  //if b[i] = 1 then:
  //C := BarrettReduction(C * A, N, );
  //A := BarrettReduction (A * A, N, );
  //return C
  def longModPowerBarret(a: LongNumber, b: LongNumber, n: LongNumber): LongNumber = {
    val mu = getMu(n)
    val binaryB = toBinaryNumLittleEndian(b)

    @tailrec
    def longLongModPowerAcc(a: LongNumber, bBinaryTail: Seq[Int], n: LongNumber, mu: LongNumber, result: LongNumber = One): LongNumber =
      if (bBinaryTail.isEmpty) result
      else {
        val aa = longMul(a, a)
        val updatedA = barrettReduction(aa, n, mu)
        if (bBinaryTail.head == 1) {
          val ca = longMul(result, a)
          val updateRes = barrettReduction(ca, n, mu)

          longLongModPowerAcc(updatedA, bBinaryTail.tail, n, mu, updateRes)
        } else longLongModPowerAcc(updatedA, bBinaryTail.tail, n, mu, result)

      }

    longLongModPowerAcc(a, binaryB, n, mu)
  }


  def longAddMod(a: LongNumber, b: LongNumber, n: LongNumber): LongNumber = {
    val mu = getMu(n)
    val redcA = barrettReduction(a, n, mu)
    val redcB = barrettReduction(b, n, mu)
    val (sum, carry) = longAdd(redcA, redcB)
    barrettReduction(sum, n, mu)
  }

  def longSubMod(a: LongNumber, b: LongNumber, n: LongNumber): LongNumber = {
    val mu = getMu(n)
    val redcA = barrettReduction(a, n, mu)
    val redcB = barrettReduction(b, n, mu)
    if (longCmpr(redcA, redcB) == 1) {
      val (diff, borr) = longSub(redcA, redcB)

      diff
    } else {
      val (diff, borr) = longSub(redcB, redcA)
      val (res, borr2) = longSub(n, diff)

      res
    }
  }

  // def longMul(thisArray: Seq[Int], thatArray: Seq[Int]): Seq[Int] = {
  //
  //    @tailrec
  //    def longMulAcc(aArray: Seq[Int], bTail: Seq[Int], i: Int = 0, cArray: Seq[Int] = Seq.fill(maxLength)(0)): Seq[Int] = {
  //      if (i < maxLength) {
  //        val (temp, mulOneDigitCarry) = longMulOneDigit(aArray, bTail.head) //temp = e0d 4e04 b0f1 4c5f 88af 02c2 b367 1765 a33a 14e0 3886
  //        val (shiftedTemp, shiftedCarry) = longShiftDigitsToHigh(temp, i)
  //        val (updatedCArray, addCarry) = longAdd(cArray, shiftedTemp)
  //        longMulAcc(aArray, bTail.tail, i + 1, updatedCArray)
  //      } else cArray
  //    }
  //
  //    longMulAcc(aArray = thisArray, bTail = thatArray)
  //  }
  def longMulMod(a: LongNumber, b: LongNumber, n: LongNumber): LongNumber = {

    @tailrec
    def longMulModAcc(a: LongNumber, bTail: Seq[Int], mu: LongNumber, n: LongNumber, result: LongNumber = Zero, i: Int = 0): LongNumber = {
      if (i < maxLength) {
        val (temp, mulOneDigitCarry) = longMulOneDigit(a, bTail.head)

        val (shiftedTemp, shiftedCarry) = longShiftDigitsToHigh(temp, i)
        val shiftedTempMod = barrettReduction(shiftedTemp, n, mu)

        val (updatedResult, addCarry) = longAdd(result, shiftedTempMod)

        longMulModAcc(a, bTail.tail, mu, n, updatedResult, i + 1)
      } else barrettReduction(result, n, mu)
    }

    val mu =  getMu(n)
    longMulModAcc(a = barrettReduction(a, n, mu), bTail = barrettReduction(b, n, mu), mu = mu, n)
  }

  def longSquareMod(a: LongNumber, n: LongNumber): LongNumber = {
    val mu = getMu(n)
    val redcA = barrettReduction(a, n, mu)
    val mul = longMul(a, a)

    barrettReduction(mul, n, mu)
  }

}
