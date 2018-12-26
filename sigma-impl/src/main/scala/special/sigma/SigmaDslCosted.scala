package special.sigma

import special.SpecialPredef
import special.collection.{Col, _}

import scala.reflect.ClassTag
import scalan.meta.RType
import scalan.{NeverInline, Reified}

class CCostedContext(val ctx: Context) extends CostedContext {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def OUTPUTS: CostedCol[Box] = dsl.costBoxes(ctx.OUTPUTS)
  def INPUTS: CostedCol[Box] = dsl.costBoxes(ctx.INPUTS)
  def HEIGHT: Costed[Int] = {
    val cost = dsl.CostModel.SelectField
    new CCostedPrim(ctx.HEIGHT, cost, 4L)
  }
  def SELF: CostedBox = new CCostedBox(ctx.SELF, dsl.CostModel.AccessBox)
  def LastBlockUtxoRootHash: CostedAvlTree = new CCostedAvlTree(ctx.LastBlockUtxoRootHash, dsl.CostModel.AccessAvlTree)
  def MinerPubKey: CostedCol[Byte] = dsl.costColWithConstSizedItem(ctx.MinerPubKey, dsl.CostModel.PubKeySize.toInt, 1)
  def getVar[T](id: Byte)(implicit cT: RType[T]): CostedOption[T] = {
    val opt = ctx.getVar(id)(cT)
    dsl.costOption(opt, dsl.CostModel.GetVar)
  }

  @NeverInline
  def getConstant[T](id: Byte)(implicit cT: RType[T]): Costed[T] = SpecialPredef.rewritableMethod

  def value = ctx
  def cost = ctx.cost
  def dataSize = ctx.dataSize
}

class CCostedBox(val box: Box, val cost: Int) extends CostedBox {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def id: CostedCol[Byte] = dsl.costColWithConstSizedItem(box.id, box.id.length, 1)
  def valueCosted: Costed[Long] = {
    val cost = dsl.CostModel.SelectField
    new CCostedPrim(box.value, cost, 8L)
  }
  def bytes: CostedCol[Byte] = dsl.costColWithConstSizedItem(box.bytes, box.bytes.length, 1)
  def bytesWithoutRef: CostedCol[Byte] = dsl.costColWithConstSizedItem(box.bytesWithoutRef, box.bytesWithoutRef.length, 1)
  def propositionBytes: CostedCol[Byte] = dsl.costColWithConstSizedItem(box.propositionBytes, box.propositionBytes.length, 1)
  def registers: CostedCol[AnyValue] = {
    val len = box.registers.length
    val costs = dsl.Cols.replicate(len, dsl.CostModel.AccessBox)
    val sizes = box.registers.map(o => o.dataSize)
    new CCostedCol(box.registers, costs, sizes, dsl.CostModel.CollectionConst)
  }
  def getReg[@Reified T](id: Int)(implicit cT:RType[T]): CostedOption[T] = {
    val opt = box.getReg(id)(cT)
    dsl.costOption(opt, dsl.CostModel.GetRegister)
  }

  @NeverInline
  def creationInfo: Costed[(Int, Col[Byte])] = SpecialPredef.rewritableMethod

  def value: Box = box
  def dataSize: Long = box.dataSize
}

class CCostedAvlTree(val tree: AvlTree, val cost: Int) extends CostedAvlTree {
  def dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  def startingDigest: CostedCol[Byte] = dsl.costColWithConstSizedItem(tree.startingDigest, dsl.CostModel.PubKeySize.toInt, 1)
  def keyLength: Costed[Int] = new CCostedPrim(tree.keyLength, dsl.CostModel.SelectField, 4)
  def valueLengthOpt: CostedOption[Int] = dsl.costOption(tree.valueLengthOpt, dsl.CostModel.SelectField)
  def maxNumOperations: CostedOption[Int] = dsl.costOption(tree.maxNumOperations, dsl.CostModel.SelectField)
  def maxDeletes: CostedOption[Int] = dsl.costOption(tree.maxDeletes, dsl.CostModel.SelectField)

  def value = tree
  def dataSize = tree.dataSize
}

