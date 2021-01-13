//import Conversions._
//import scala.language.postfixOps
//import GF_old._
//
//
//
//
//
//class GF_old(val polynom: Seq[Int])  {
//
//  def maxPower: Int = GF_old.generatorSize - 1
//
//  val binary = binaryVectorLittleEndian
//  def binaryVectorLittleEndian: Seq[Int] = {
//    val powersSet = polynom.toSet
//
//    (0 to maxPower).map(i => if (powersSet contains i) 1 else 0)
//  }
//
//  def toLong16: Seq[Int] = {
//    val num = binaryVectorLittleEndian.grouped(Long16.basisPower).map(digit => Integer.valueOf(digit.reverse.mkString, 2).toInt)
//    num.toSeq ++ Seq.fill(Long16.maxLength - num.length)(0)
//  }
//
//  def << (shift: Int): GF_old = {
//    new GF_old(this.polynom.map(_ + shift))
//  }
//
//  def binaryLength: Int = binary.length
//
//  def mod(generator: Seq[Int]): GF_old = {
//    val genGF = new GF_old(generator)
//    var result = GF_old(this)
//    var temp = new GF_old(Seq())
//    var copy = Zero
//    var i = result.binaryLength - 1
//    while (i > genGF.binaryLength) {
//      if (result.binary(i) == 1) {
//        temp = genGF << (i - (genGF.binaryLength - 1))
//        copy = result + temp
//        result = copy
//      }
//      i -= 1
//    }
//    new GF_old(result.polynom.filter(_ < genGF.binaryLength))
//  }
//
//  def +(that: GF_old): GF_old = {
//    val a = this.binary
//    val b = that.binaryVector
//
//    GF_old((a zip b).map{ case (l, r) => l ^ r})
//  }
//
//  def *(that: GF_old): GF_old = {
//    val a = this.polynom
//    val b = that.polynom
//
//    val result = a.flatMap(degA =>
//      b.map(degB => degA + degB)
//    )
//      .zipWithIndex
//      .groupBy(p => p._1)
//      .map { case (k, v) => k -> (v.length & 1) }
//      .filter { case (k, v) => v == 1 && k <= maxPower}
//      .keys
//      .toSeq.sortBy(-_)
//
//    new GF_old(result)
//  }
//
//  def square: GF_old = {
//    val result = new GF_old(polynom.map(_ * 2))
//    result.mod(GF_old.generatorBigEndian)
//  }
//
//  def trace: GF_old = {
//    var buff =  GF_old(this)
//    var result =  new GF_old(Seq())
//    var i = 0
//    while ( {
//      i < GF_old.generatorSize
//    }) {
//      buff = buff.square
//      result = result + buff
//      i += 1
//    }
//    result
//  }
//
//  def inverse: GF_old = {
//    var buff = GF_old(this)
//    var result = GF_old(this)
//    var i = 0
//    while (i < GF_old.generatorSize - 2) {
//      buff = buff.square
//      result = result  + buff
//      i += 1
//    }
//    result.square
//  }
//
//  def pow(binary: Seq[Int]): GF_old = {
//    var result = new GF_old(Seq(0))
//    var A = GF_old(this)
//    var i = binary.length - 1
//    while (i >= 0) {
//      if (binary(i) == 1) result = (A * result) mod(GF_old.generatorBigEndian)
//      A = (A square) mod GF_old.generatorBigEndian
//      i -= 1
//    }
//    result
//  }
//
//  def ^(binary: String): GF_old = {
//    var result = new GF_old(Seq(0))
//    var A = GF_old(this)
//    var i = binary.length - 1
//    while (i >= 0) {
//      if (binary.charAt(i) == '1') result = (A * result) mod GF_old.generatorBigEndian
//      A = (A square) mod GF_old.generatorBigEndian
//      i -= 1
//    }
//    result
//  }
//
//  override def toString: String = binary.reverseIterator.mkString
//
//}
//
//object GF_old {
//
//  val generatorSize = 3
//  val generator = Seq(3, 1, 0)
//
//
//  val Zero: GF_old = new GF_old(Seq())
//  val One: GF_old = new GF_old(Seq(0))
//
////  def fromLong16(a: LongNumber): GF_old = GF_old(a.toBinaryString)
//
//  def apply(gf: GF_old): GF_old =
//    new GF_old(gf.polynom)
//
//  def apply(binaryArr: Seq[Int]): GF_old =
//    new GF_old(binaryArr.reverseIterator.zipWithIndex.withFilter(_._1 == 1).map(_._2).toSeq.distinct)
//
//  def apply(binaryString: String): GF_old =
//    GF_old(binaryString.map(c => Integer.valueOf(c.toString).toInt))
//}
