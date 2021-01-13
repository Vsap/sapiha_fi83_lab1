import Long16.radix

import scala.annotation.tailrec



//class Long16 private (array: Seq[Int])  {
//
//  val basisPower = Long16.basisPower
//  val w = basisPower
//  val moduloBasis = math.pow(2, basisPower).toInt
//  val maxLength = Long16.maxLength
//
//  // little endian
//  private val numArray: Seq[Int] = array
//}
object Long16 {

  private val alphabet = Map(
    '0' -> 0,
    '1' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'a' -> 10,
    'b' -> 11,
    'c' -> 12,
    'd' -> 13,
    'e' -> 14,
    'f' -> 15
  )

  val basisPower = 16
  val W = basisPower
  val radix = 1 << basisPower
  val digitMaxValue = radix - 1

  val maxLength = 256

  val Zero = Seq.fill(maxLength)(0)
  val One = 1 +: Zero.init
  val Two = 2 +: Zero.init
  val Three = 3 +: Zero.init
  val Four = 4 +: Zero.init
  val Five = 5 +: Zero.init
  val MaxValue = Seq.fill(maxLength)(digitMaxValue)
  //
  //  def apply(fromString: String): Long16 = {
  //    val stringArray = fromString.reverseIterator
  //    val array: Seq[Int] = if(stringArray.size < Long16.maxLength) {
  //      val accMap = stringArray.zipWithIndex.map(_.swap).toMap
  //      Seq.fill(maxLength)(0).zipWithIndex.map(i => accMap.getOrElse(i._2, 0))
  //    } else stringArray.take(Long16.maxLength).map(_.toInt).toSeq
  //    new Long16(array)
  //  }
  //
  //  def apply(singleDigit: Int = 0): Long16 = {
  //    assert(singleDigit <= (Int.MaxValue << 1))
  //    new Long16(singleDigit +: Seq.fill(maxLength - 1)(0))
  //  }

  //  def copy(thatArray: Seq[Int]): Seq[Int] =

  //  carry := 0;
  //  for i := 0 to n-1 do:
  //  temp := a[i] + b[i] + carry;
  //  c[i] := temp & (2^w – 1); // чому саме так?
  //  carry := temp >> w;
  //  return C, carry
  def longAdd(thisArray: Seq[Int], thatArray: Seq[Int]): (Seq[Int], Int) = {
    @tailrec
    def longAddAcc(i: Int = 0, carry: Int = 0, cReversed: Seq[Int] = Seq(), aTail: Seq[Int], bTail: Seq[Int]): (Seq[Int], Int) =
      if (i < maxLength) {
        val temp = aTail.head + bTail.head + carry
        val ci = temp & digitMaxValue
        val newCarry = temp >> W

        longAddAcc(i + 1, newCarry, ci +: cReversed, aTail.tail, bTail.tail)
      } else (cReversed.reverse, carry)

    longAddAcc(aTail = thisArray, bTail = thatArray)
  }

  //  borrow := 0;
  //  for i := 0 to n-1 do:
  //  temp := a[i] - b[i] - borrow;
  //  if temp >= 0 then:
  //    c[i] := temp;
  //  borrow := 0;
  //  else:
  //  c[i] := 2w + temp;
  //  borrow := 1;
  //  return C, borrow
  def longSub(thisArray: Seq[Int], thatArray: Seq[Int]): (Seq[Int], Int) = {
    @tailrec
    def longSubAcc(i: Int = 0, borrow: Int = 0, cReversed: Seq[Int] = Seq(), aTail: Seq[Int], bTail: Seq[Int]): (Seq[Int], Int) =
      if (i < maxLength) {
        val temp = aTail.head - bTail.head - borrow
        if (temp < 0) {
          val ci = radix + temp
          val newBorrow = 1
          longSubAcc(i + 1, newBorrow, ci +: cReversed, aTail.tail, bTail.tail)
        } else {
          val ci = temp
          val newBorrow = 0
          longSubAcc(i + 1, newBorrow, ci +: cReversed, aTail.tail, bTail.tail)
        }
      } else (cReversed.reverse, borrow)

    longSubAcc(aTail = thisArray, bTail = thatArray)
  }

  //  i := n-1;
  //  while (a[i] = b[i]) do:
  //  i := i-1;
  //  if (i = -1) then: // всі цифри однакові
  //  return 0;
  //  else:
  //  if a[i] > b[i] then:
  //  return 1
  //  else:
  //  return -1;
  def  longCmpr(thisArray: Seq[Int], thatArray: Seq[Int]): Int = {
    @tailrec
    def longCmprAcc(i: Int = 0, aTailBigEndian: Seq[Int], bTailBigEndian: Seq[Int]): Int =
      if (i < maxLength) {
        if (aTailBigEndian.head == bTailBigEndian.head) longCmprAcc(i + 1, aTailBigEndian.tail, bTailBigEndian.tail)
        else if (aTailBigEndian.head > bTailBigEndian.head) 1
        else -1
      } else 0

    longCmprAcc(aTailBigEndian = thisArray.reverse, bTailBigEndian = thatArray.reverse)

    //    val (eq, unEq) = (numArray zip thatArray).reverseIterator.span(p => p._1 == p._2)
    //    if (unEq.isEmpty) 0
    //    else if ({ val (aDigit, bDigit) = unEq.next(); aDigit > bDigit}) 1
    //    else -1

  }

  //  carry := 0;
  //  for i := 0 to n-1 do:
  //      temp := a[i] * b + carry;
  //      c[i] := temp & (2^w – 1);
  //      carry := temp >> w; // скільки значущих біт містить carry?
  //  C[n] := carry;
  //  return C
  def longMulOneDigit(thisArray: Seq[Int], thatArrayDigit: Int): (Seq[Int], Int) = {
    @tailrec
    def longMulOneDigitAcc(aTail: Seq[Int], bDigit: Int, i: Int = 0, carry: Int = 0, cBigEndian: Seq[Int] = Seq()): (Seq[Int], Int) =
      if (i < maxLength) {
        val temp: Long = aTail.head.toLong * bDigit.toLong + carry.toLong // temp = 38 = 0x26
        val ci = (temp & digitMaxValue).toInt // eq temp mod digitMaxValue (temp & (2^W - 1))
        val newCarry = (temp >> W).toInt

        longMulOneDigitAcc(aTail.tail, thatArrayDigit, i + 1, newCarry, ci +: cBigEndian)
      } else (cBigEndian.reverse, carry)

    longMulOneDigitAcc(aTail = thisArray, bDigit = thatArrayDigit)
  }

  def longMulOneDigitNoCarry(thisArray: Seq[Int], digit: Int): Seq[Int] = {
    @tailrec
    def longMulOneDigitAcc(i: Int = 0, carry: Int = 0, cBigEndian: Seq[Int] = Seq(), aTail: Seq[Int], bDigit: Int): Seq[Int] =
      if (i < maxLength) {
        val temp: Long = aTail.head.toLong * bDigit.toLong + carry.toLong
        val ci = (temp & digitMaxValue).toInt
        val newCarry = (temp >> W).toInt

        longMulOneDigitAcc(i + 1, newCarry, ci +: cBigEndian, aTail.tail, bDigit)
      } else (carry +: cBigEndian).reverse

    longMulOneDigitAcc(aTail = thisArray, bDigit = digit)
  }


  def longShiftDigitsToHigh(thatArray: Seq[Int], shift: Int = 1): (Seq[Int], Seq[Int]) =
    Seq.fill(shift)(0) ++ thatArray.take(maxLength - shift) -> thatArray.takeRight(shift)

  def longShiftDigitsToLow(thatArray: Seq[Int], shift: Int = 1): (Seq[Int], Seq[Int]) =
    thatArray.takeRight(maxLength - shift) ++ Seq.fill(shift)(0) -> thatArray.take(shift)

  // LongShiftDigitsToHigh, яка зсуває комірки масиву цифр у бік старших індексів.
  @inline
  def longShiftDigitsToHigh(thatArray: Seq[Int]): (Seq[Int], Int) = (0 +: thatArray.init) -> thatArray.last


  // TODO: make return correct shifted digits
  def longShiftBitsToHigh(thatArray: Seq[Int], shift: Int): (Seq[Int], Seq[Int]) = {

    val initBitShift = shift & (W - 1)
    val digitShift = shift >> 4 //Math.log(W).toInt //TODO: add explanation

    val initShiftBitsArray = Seq[Int]()
    val initCarryBit = 0

    val (shiftedArrayReversed, carryBits) = thatArray.foldLeft((initShiftBitsArray, initCarryBit)){ case ((shiftedArr, prevCarryBits), elem) =>
      val tempShifted = (elem << initBitShift) + prevCarryBits
      val (shiftedElem, nextCarry) = (tempShifted & digitMaxValue) -> (tempShifted >> W)
      (shiftedElem +: shiftedArr) -> nextCarry
    }

    val (digitShiftedArray, shiftedDigits) = longShiftDigitsToHigh(shiftedArrayReversed.reverse, digitShift)

    digitShiftedArray -> Seq() // -> longShiftBitsToHigh(shiftedDigits) + carryBits
  }

  // TODO: make return correct shifted digits
  def longShiftBitsToLow(thatArray: Seq[Int], shift: Int): Seq[Int] = {

    val (shiftedBits, result) = (0 until shift).foldLeft((Seq.empty[Int], thatArray)){ case ((shiftedAcc, acc), i) =>
      shiftedAcc -> longShiftBitToLow(acc)
    }

    result
  }

  // TODO: Check division by 2
  def longShiftBitToLow(thatArray: Seq[Int]): Seq[Int] = {
    (thatArray :+ 0).reverseIterator
      .sliding(2)
      .map { case Seq(higherDigit, currentDigit) =>
        val borrowBit = (higherDigit & 1)

        (currentDigit >> 1) | (borrowBit << W)
      }.toSeq.reverse
  }

  //  C := 0;
  //  for i := 0 to n-1 do:
  //    temp := LongMulOneDigit(A, b[i]);
  //    LongShiftDigitsToHigh(temp, i);
  //    C := C + temp; // багаторозрядне додавання!
  //  return C
  def longMul(thisArray: Seq[Int], thatArray: Seq[Int]): Seq[Int] = {

    // e0d 4e04 b0f1 4c5f 88af 02c2 b367 1765 a33a 14e0 3886
    @tailrec
    def longMulAcc(aArray: Seq[Int], bTail: Seq[Int], i: Int = 0, cArray: Seq[Int] = Seq.fill(maxLength)(0)): Seq[Int] = {
      if (i < maxLength) {
        val (temp, mulOneDigitCarry) = longMulOneDigit(aArray, bTail.head) //temp = e0d 4e04 b0f1 4c5f 88af 02c2 b367 1765 a33a 14e0 3886
        val (shiftedTemp, shiftedCarry) = longShiftDigitsToHigh(temp, i)
        val (updatedCArray, addCarry) = longAdd(cArray, shiftedTemp)
        longMulAcc(aArray, bTail.tail, i + 1, updatedCArray)
      } else cArray
    }

    longMulAcc(aArray = thisArray, bTail = thatArray)
  }


  def longBitLength(thatArray: Seq[Int]): Int = {
    val (highestNumber, index) = getHighestNonEmptyElementWithIndex(thatArray).getOrElse((0,0))
    if (index == 0) (Math.log(highestNumber) / Math.log(2)).toInt + 1
    else (Math.log(highestNumber) / Math.log(2)).toInt + (index) * basisPower + 1

  }

  def longDigitLength(thatArray: Seq[Int]): Int =
    getHighestNonEmptyElementWithIndex(thatArray).map{case (elem, index) => index + 1}.getOrElse(0)


  def getHighestNonEmptyElementWithIndex(thatArray: Seq[Int]): Option[(Int, Int)] = {
    thatArray.zipWithIndex.reverseIterator
      .dropWhile(_._1 == 0).toSeq
      .headOption
  }
  //  k := BitLength(B);
  //  R := A;
  //  Q := 0;
  //  while R >= B do: // багаторозрядне порівняння!
  //  t := BitLength(R);
  //  C := LongShiftBitsToHigh(B, t – k);
  //  if R < C then: // багаторозрядне порівняння! вийшло забагато?
  //      t := t – 1; // тоді повертаємось на біт назад
  //      C := LongShiftBitsToHigh(B, t – k);
  //  R := R – C;
  //  Q := Q + 2^(t – k); // встановити в Q біт із номером (t – k)
  //  return Q, R
  def longDivMod(thisArray: Seq[Int], thatArray: Seq[Int]): (Seq[Int], Seq[Int]) = {

    @tailrec
    def longDivModAcc(thisArray: Seq[Int],
                      thatArray: Seq[Int],
                      thatBitLength: Int, // val k = bitLength(thatArray)
                      rArray: Seq[Int] = thisArray,
                      qArray: Seq[Int] = Zero): (Seq[Int], Seq[Int]) = {
      if (longCmpr(rArray, thatArray) > -1) {
        val t = longBitLength(rArray)
        val (cArray, shiftCarryBits) = longShiftBitsToHigh(thatArray, t - thatBitLength)
        val (cArrayAugmented, tAugmented) =
          if (longCmpr(rArray, cArray) < 0) {
            val (cArrayAugmented, carryBits) = longShiftBitsToHigh(thatArray, t - 1 - thatBitLength)
            cArrayAugmented -> (t - 1)
          }
          else cArray -> t

        val (rArrayAugmented, borrow) = longSub(rArray, cArrayAugmented)
        val (bitToSetup, shiftCarryBits2) = longShiftBitsToHigh(One, tAugmented - thatBitLength)
        val (qArrayAugmented, addCarry) = longAdd(qArray, bitToSetup)

        longDivModAcc(thisArray, thatArray, thatBitLength, rArrayAugmented, qArrayAugmented)
      }
      else qArray -> rArray
    }

    if (longCmpr(thisArray, Zero) == 0) Zero -> Zero
    else if (longCmpr(thatArray, Zero) == 0) {
      throw new IllegalArgumentException("Incorrect B number: " + thatArray)
      (Zero, Zero)
    }
    else
      longDivModAcc(thisArray, thatArray, longBitLength(thatArray))
  }

  def longSquare(thisArray: Seq[Int]): Seq[Int] = ???

  def toBinaryNumLittleEndian(thisArray: Seq[Int]): Seq[Int] = {

    def reduceDigitToLittleEndianBinaryArray(digit: Int): Seq[Int] = {
      for (
        //        i <- 0 until (Math.log(digitMaxValue) / Math.log(2)).toInt
        i <- 0 until W
      ) yield (digit & (1 << i)) >> i
    }

    thisArray
      .flatMap(digit => reduceDigitToLittleEndianBinaryArray(digit))
      .reverse
      .dropWhile(_ == 0)
      .reverse

  }

  //  C := 1;
  //  for i := 0 to m-1 do:
  //  if b[i] = 1 then:
  //    C := C * A; // багаторозрядне множення!
  //  A := A * A; // багаторозрядне множення!
  //  return C
  def longPower1(thisArray: Seq[Int], thatArray: Seq[Int]): Seq[Int] = {

    @tailrec
    def longPower1Acc(thisArrayAcc: => Seq[Int],
                      thatArrayBinaryTail: Seq[Int],
                      result: => Seq[Int] = One): Seq[Int] =
      if (thatArrayBinaryTail.isEmpty) result
      else if (thatArrayBinaryTail.head == 1) {
        val updatedResult = longMul(result, thisArrayAcc)
        val thisArrayAccSquared = longMul(thisArrayAcc, thisArrayAcc)
        longPower1Acc(thisArrayAccSquared, thatArrayBinaryTail.tail, updatedResult)
      } else {
        val thisArrayAccSquared = longMul(thisArrayAcc, thisArrayAcc)
        longPower1Acc(thisArrayAccSquared, thatArrayBinaryTail.tail, result)
      }

    val thatBinaryArray = toBinaryNumLittleEndian(thatArray)

    longPower1Acc(thisArray, thatBinaryArray)
  }

  //  C := 1;
  //  D[0] := 1; D[1] := A; // D[] – таблиця степенів А
  //  for i := 2 to (2t – 1) do: // передобчислення
  //  D[i] := D[i – 1] * A;
  //  for i := m-1 to 0 do:
  //  C := C * D[b[i]]; // багаторозрядне множення!
  //  if i <> 0 then: // на останньому кроці до квадратів не підносимо
  //  for k := 1 to t do: // підносимо до квадрату t разів
  //  C := C * C; // багаторозрядне множення!
  //
  //  return C
  def longPowerWindow(thisArray: Seq[Int], thatArray: Seq[Int]): Seq[Int] = ???
}
