import Long16.radix


object Conversions {

  implicit class Long16Opt(val num: Seq[Int]) extends AnyVal {
    def toHexString: String = Conversions.toHexString(num)
  }
  implicit class Long1Opt(val binaryNum: Seq[Int]) extends AnyVal {
    def toBinaryString: String = Conversions.toBinaryString(binaryNum)
  }


  def convertHexToDigit(input: String): Int = Integer.parseInt(input.take(4), radix)

  def convertDigitToHexChar(digit: Int): String = {
    val hexString = Integer.toHexString(digit)
    Seq.fill((Long16.basisPower / 4) /* max length of Hex Digits for one Long16 digit*/ - hexString.length)(0).mkString ++ hexString
  }

  def convertDigitToBinary(digit: Int): String = {
    val binaryString = Integer.toBinaryString(digit)
    Seq.fill(Long16.basisPower /* max length of Binary Digits for one Long16 digit*/ - binaryString.length)(0).mkString ++ binaryString
  }

  def toHexString(thisArray: Seq[Int]): String =
    if (Long16.longCmpr(thisArray, Long16.Zero) == 0)
      thisArray.reverseIterator
        .map(convertDigitToHexChar)
        .mkString
    else thisArray.reverseIterator
      .dropWhile(_ == 0)
      .map(convertDigitToHexChar)
      .mkString
      .dropWhile(_ == '0')

  def toBinaryString(binaryArray: Seq[Int]): String = binaryArray
    .mkString
    .reverse
    .dropWhile(_ == '0')

  def fromHexString(fromString: String): Seq[Int] = {
    val array = fromString.toLowerCase.reverseIterator
      .take(Long16.maxLength * Long16.basisPower / 4 /* Math.log(16) */)
      .grouped(Long16.basisPower / 4 /* Math.log(Hex) */)
      .map(c =>
        Integer.parseInt(c.mkString.reverse, Long16.basisPower))
      .toSeq

    array ++ Seq.fill(Long16.maxLength - array.length)(0)
  }

}

