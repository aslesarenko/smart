package special.sigma

import special.collection.{Col, _}
import scalan.Reified
import scalan.meta.RType

trait CostedSigmaObject[Val] extends Costed[Val] {
  def dsl: SigmaDslBuilder
  def builder: CostedBuilder = dsl.Costing
}

trait CostedContext extends CostedSigmaObject[Context] {
  def OUTPUTS: CostedCol[Box]
  def INPUTS: CostedCol[Box]
  def HEIGHT: Costed[Int]
  def SELF: CostedBox
  def LastBlockUtxoRootHash: CostedAvlTree
  def MinerPubKey: CostedCol[Byte]
  def getVar[T](id: Byte)(implicit cT: RType[T]): CostedOption[T]
  def getConstant[T](id: Byte)(implicit cT: RType[T]): Costed[T]
}

trait CostedBox extends CostedSigmaObject[Box] {
  def id: CostedCol[Byte]
  def valueCosted: Costed[Long]
  def bytes: CostedCol[Byte]
  def bytesWithoutRef: CostedCol[Byte]
  def propositionBytes: CostedCol[Byte]
  def registers: CostedCol[AnyValue]
  def getReg[@Reified T](id: Int)(implicit cT:RType[T]): CostedOption[T]
  def creationInfo: Costed[(Long, Col[Byte])]
}

trait CostedAvlTree extends CostedSigmaObject[AvlTree] {
  def startingDigest: CostedCol[Byte]
  def keyLength: Costed[Int]
  def valueLengthOpt: CostedOption[Int]
  def maxNumOperations: CostedOption[Int]
  def maxDeletes: CostedOption[Int]
}
