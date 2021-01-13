import Conversions._

import scala.language.postfixOps
import NB._

import scala.annotation.tailrec




class NB private[NB](binaryArrLE: Seq[Int]) {

//  def maxPower: Int = NB.generatorSize - 1

  //val polynom: Seq[Int] = binaryArrLE.zipWithIndex.withFilter(_._1 == 1).map(_._2).distinct
  val binaryVector: Seq[Int] = binaryArrLE //++ Seq.fill(0)(NB.generatorBigEndian.max - 1 - binaryArrLE.length)

//  def binaryVectorLittleEndian: Seq[Int] = {
//    val powersSet = polynom.toSet
//    (0 to maxPower).map(i => if (powersSet contains i) 1 else 0)
//  }


  // ---------NB-------------
  def >>>(n: Int): NB = {
    val (tail, shifted) = binaryVector.splitAt(binaryVector.length - n)
    NB.fromBinaryArray(shifted ++ tail)
  }

  def <<<(n: Int): NB = {

    NB.fromBinaryArray(<<<(binaryVector, n))
  }

  def plusNB(b: NB): NB = {
    NB.fromBinaryArray(
      (binaryVector zip b.binaryVector).map(p => p._1 ^ p._2)
    )
  }

  def mulNB(that: NB): NB = {
    val res = for (i <- 0 until NB.generatorBigEndian.max) yield {
      val u = <<<(this.binaryVector, i)
      val v = <<<(that.binaryVector, i)
      val uMM = mulMM(u, MM)

      (uMM zip v).foldLeft(0){ case (acc, p) => (acc + (p._1 * p._2)) & 1}
    }

    NB.fromBinaryArray(res)
  }

  def squaredNB(): NB = this >>> 1

  def powNB(n: Int): NB = {
    val binaryN = Integer.toBinaryString(n)
    var res = NB.One
    for {
      i <- binaryN.indices
    } {
      if (binaryN.charAt(i) == '1') res = this.mulNB(res)
      if (i != (binaryN.length - 1)) res = res.squaredNB()
    }

    res
  }

  def inverseNB: NB = {
    val m = NB.generatorBigEndian.max
    val n = (1 << m) - 2
    this.powNB(n)
  }

  def traceNB: Int =
    binaryVector.foldLeft(0){ case (acc, i) => (acc + i) & 1 }

  def setCellNB(value: Int, atIndex: Int): NB = {
    NB.fromBinaryArray(binaryVector.zipWithIndex.map { case (bit, index) => if (index == atIndex) value else bit})
  }

  private def mulMM(vector: Seq[Int], mm: Array[Array[Int]] = MM): Seq[Int] = {
    (0 until NB.generatorBigEndian.max)
      .map(j =>
        (0 until NB.generatorBigEndian.max)
          .map(i => vector(i) * mm(i)(j))
          .sum & 1
      )
  }
  private def <<<(vector: Seq[Int], n: Int): Seq[Int] = {
    val (shifted, tail) = vector.splitAt(n)
    tail ++ shifted
  }

  override def toString: String = {
    val string = binaryVector.dropWhile(_ == 0).mkString
    if (string.isEmpty) "0" else string
  }

}

object NB {

//  val generatorSize = 3
//  val generatorString = ""

//  val generatorBigEndian = Seq(443, 28, 3, 1, 0)
  val generatorBigEndian = Seq(3, 2, 0)
//  val generatorNBLitleEndian = Seq(1, 1, 0, 1)

  lazy val generatorBinaryVectorLittleEnd: Seq[Int] = {
    val maxPow = generatorBigEndian.headOption.getOrElse(0)
    val powersSet = generatorBigEndian.toSet
    (0 to maxPow).map(i => if (powersSet contains i) 1 else 0)
  }
  lazy val generatorBinaryBE: Seq[Int] = generatorBinaryVectorLittleEnd.reverse

  val Zero: NB = NB.fromBinaryArray(Seq(0))
  val One: NB = NB.fromBinaryArray(Seq.fill(generatorBigEndian.max)(1))

  lazy val MM = {
    val size = NB.generatorBigEndian.max
    val p = NB.generatorBigEndian.max * 2 + 1
    val arr = Array.ofDim[Int](size, size)
    for {
      i <- 0 until size
      j <- 0 until size
    }{
      val i2 = 1 << i
      val j2 = 1 << j
      val bit = if (Math.floorMod((i2 + j2), p) == 1) 1
      else if (Math.floorMod((i2 - j2), p) == 1) 1
      else if (Math.floorMod((j2 - i2), p) == 1) 1
      else if (Math.floorMod((-i2 - j2), p) == 1) 1
      else 0

      arr(i)(j) = bit
    }

    arr

  }


  //  def fromLong16(a: LongNumber): GF_old = GF_old(a.toBinaryString)

  def apply(gf: NB): NB = new NB(gf.binaryVector)


  def fromPolynom(polynom: Seq[Int]): NB = {
    if(polynom.nonEmpty) {
      val powersSet = polynom.toSet
      val maxPower = generatorBigEndian.head - 1
      NB.fromBinaryArray((0 to polynom.max).map(i => if (powersSet contains i) 1 else 0))
    } else NB.Zero
  }

  def fromNBString(binaryStringNB: String): NB = {
    val binaryArrNB: Seq[Int] = binaryStringNB.map(c => Integer.valueOf(c.toString))
    NB.fromBinaryArray(binaryArrNB)
  }

  def fromBinaryArray(binaryArrNB: Seq[Int]): NB = {
    new NB(binaryArrNB)
  }

//  private def modRedc(binArrBigEnd: Seq[Int]): Seq[Int] = {
//    @tailrec
//    def modGenAcc(binABigEnd: Seq[Int], binModBigEnd: Seq[Int]): Seq[Int] = {
//      val tempABE = binABigEnd.dropWhile(_ == 0)
//      if (tempABE.isEmpty) Seq(0)
//      if (tempABE.length < binModBigEnd.length) tempABE
//      else {
//        val head = (tempABE zip binModBigEnd).map(p => p._1 ^ p._2)
//        val tail = tempABE.drop(head.length)
//        modGenAcc(head ++ tail, binModBigEnd)
//      }
//    }
//    modGenAcc(binArrBigEnd, NB.generatorBinaryBE).reverse
//  }
}
