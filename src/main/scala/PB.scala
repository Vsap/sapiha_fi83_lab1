import Conversions._

import scala.language.postfixOps
import PB._

import scala.annotation.tailrec




class PB private[PB](binaryArrLE: Seq[Int]) {

  def maxPower: Int = PB.generatorSize - 1

  val polynom = binaryArrLE.zipWithIndex.withFilter(_._1 == 1).map(_._2).distinct
  val binaryVector: Seq[Int] = binaryArrLE ++ Seq.fill(0)(PB.generatorBigEndian.max - 1 - binaryArrLE.length)

  def binaryVectorLittleEndian: Seq[Int] = {
    val powersSet = polynom.toSet
    (0 to maxPower).map(i => if (powersSet contains i) 1 else 0)
  }

  //-------------PB-----------

  def plusPB(b: PB): PB = {
    PB.fromBinaryArray(
      (binaryVector zip b.binaryVector).map(p => p._1 ^ p._2)
    )
  }

  def mulPB(that: PB): PB = {
    val a = this.polynom
    val b = that.polynom

    val result: Seq[Int] = a.flatMap(degA =>
      b.map(degB => (degA + degB) -> 1)
    )
      .groupBy(p => p._1)
      .mapValues(v => v.length & 1 )
      .filter { case (k, v) => v == 1}
      .keys
      .toList

    PB.fromPolynom(result)
  }

  def squaredPB(): PB = PB.fromBinaryArray(binaryVector.flatMap(bit => Seq(0, bit)).tail)


  def powPB(n: Int): PB = {
    val binaryN = Integer.toBinaryString(n)
    var res = PB.One
    for {
      i <- binaryN.indices
    } {
      if (binaryN.charAt(i) == '1') res = this.mulPB(res)
      if (i != (binaryN.length - 1)) res = res.squaredPB()
    }

    res
  }

  // ---------NB-------------
  def <<<(n: Int): PB = {
    val (tail, shifted) = binaryVector.splitAt(binaryVector.length - n)
    PB.fromBinaryArray(shifted ++ tail)
  }

  def plusNB(b: PB): PB = {
    PB.fromBinaryArray(
      (binaryVector zip b.binaryVector).map(p => p._1 ^ p._2)
    )
  }

  def mulNB(that: PB): PB = {
    val a = this.polynom
    val b = that.polynom

    val result: Seq[Int] = a.flatMap(degA =>
      b.map(degB => (degA + degB) -> 1)
    )
      .groupBy(p => p._1)
      .mapValues(v => v.length & 1 )
      .filter { case (k, v) => v == 1}
      .keys
      .toList

    PB.fromPolynom(result)
  }

  def squaredNB(): PB = PB.fromBinaryArray(binaryVector.flatMap(bit => Seq(0, bit)).tail)

  def powNB(n: PB): PB = {
    //
    //    @tailrec
    //    def powAcc(a: Seq[Int], nBinary: Seq[Int], iter: Int, res: Seq[Int]): Seq[Int] = {
    //      if (iter < nBinary.length) ???
    //      else ???
    //    }
    ???
  }


  def inversePB: PB = {
    val m = PB.generatorBigEndian.max
    val n = (1 << m) - 2
    this.powPB(n)
  }

  def tracePB: Int =
    binaryVector.foldLeft(0){ case (acc, i) => (acc + i) & 1 }


  def setCellNB(value: Int, atIndex: Int): PB = {
    PB.fromBinaryArray(binaryVector.zipWithIndex.map { case (bit, index) => if (index == atIndex) value else bit})
  }





 override def toString: String = {
   val string = binaryVector.reverseIterator.dropWhile(_ == 0).mkString
   if (string.isEmpty) "0" else string
 }

}

object PB {

  val generatorSize = 3
  val generatorString = ""
  val generatorBigEndian = Seq(3, 2, 0)

  lazy val generatorBinaryVectorLittleEnd: Seq[Int] = {
    val maxPow = generatorBigEndian.headOption.getOrElse(0)
    val powersSet = generatorBigEndian.toSet
    (0 to maxPow).map(i => if (powersSet contains i) 1 else 0)
  }

  lazy val generatorBinaryBE: Seq[Int] = generatorBinaryVectorLittleEnd.reverse

  val Zero: PB = PB.fromBinaryArray(Seq(0))
  val One: PB = PB.fromBinaryArray(1 +: Seq.fill(0)(generatorBigEndian.max - 1 - 1))

  //  def fromLong16(a: LongNumber): GF_old = GF_old(a.toBinaryString)

  def apply(gf: PB): PB = new PB(gf.binaryVector)


  def fromPolynom(polynom: Seq[Int]): PB = {
    if(polynom.nonEmpty) {
      val powersSet = polynom.toSet
      val maxPower = generatorBigEndian.head - 1
      PB.fromBinaryArray((0 to polynom.max).map(i => if (powersSet contains i) 1 else 0))
    } else PB.Zero
  }

  def fromString(binaryStringBE: String): PB = {
    val binaryArrBE: Seq[Int] = binaryStringBE.reverse.map(c => Integer.valueOf(c.toString).toInt)
    PB.fromBinaryArray(binaryArrBE)
  }
  def fromBinaryArray(binaryArrLE: Seq[Int]): PB = {
    new PB(modRedc(binaryArrLE.reverse))
  }

  private def modRedc(binArrBigEnd: Seq[Int]): Seq[Int] = {
    @tailrec
    def modGenAcc(binABigEnd: Seq[Int], binModBigEnd: Seq[Int]): Seq[Int] = {
      val tempABE = binABigEnd.dropWhile(_ == 0)
      if (tempABE.isEmpty) Seq(0)
      if (tempABE.length < binModBigEnd.length) tempABE
      else {
        val head = (tempABE zip binModBigEnd).map(p => p._1 ^ p._2)
        val tail = tempABE.drop(head.length)
        modGenAcc(head ++ tail, binModBigEnd)
      }
    }
    modGenAcc(binArrBigEnd, PB.generatorBinaryBE).reverse
  }
}
