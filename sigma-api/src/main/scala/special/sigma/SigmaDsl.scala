package special.sigma

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import special.SpecialPredef

import scala.reflect.ClassTag
import special.collection._

import scalan._
import scalan.meta.RType
import scalan.Internal
import scalan.meta.RType

@scalan.Liftable
trait CostModel {
  def AccessBox: Int //= "AccessBox: Context => Box"
  def AccessAvlTree: Int //= "AccessAvlTree: Context => AvlTree"

  def GetVar: Int // = "ContextVar: (Context, Byte) => Option[T]"
  def DeserializeVar: Int // = "DeserializeVar: (Context, Byte) => Option[T]"

  def GetRegister: Int // = "AccessRegister: (Box,Byte) => Option[T]"
  def DeserializeRegister: Int // = "DeserializeRegister: (Box,Byte) => Option[T]"

  def SelectField: Int // = "SelectField"
  def CollectionConst: Int // = "Const: () => Array[IV]"
  def AccessKiloByteOfData: Int // = "AccessKiloByteOfData"
  @Reified("T") def dataSize[T](x: T)(implicit cT: ClassTag[T]): Long
  /** Size of public key in bytes */
  def PubKeySize: Long = 32
}

trait DslBuilder {}
trait DslObject {
  def builder: SigmaDslBuilder
}

@scalan.Liftable
trait SigmaProp extends DslObject {
  def isValid: Boolean
  def propBytes: Col[Byte]
  @OverloadId("and_sigma") def &&(other: SigmaProp): SigmaProp
  @OverloadId("and_bool")  def &&(other: Boolean): SigmaProp
  @OverloadId("or_sigma") def ||(other: SigmaProp): SigmaProp
  @OverloadId("or_bool")  def ||(other: Boolean): SigmaProp
  def lazyAnd(other: => SigmaProp): SigmaProp
  def lazyOr(other: => SigmaProp): SigmaProp
}

@scalan.Liftable
trait AnyValue {
  def dataSize: Long
}

@scalan.Liftable
trait Box extends DslObject {
  def id: Col[Byte]
  def value: Long
  def bytes: Col[Byte]
  def bytesWithoutRef: Col[Byte]
  def propositionBytes: Col[Byte]
  def cost: Int
  def dataSize: Long
  def registers: Col[AnyValue]

  def getReg[@Reified T](i: Int)(implicit cT: RType[T]): Option[T]

  /** Mandatory: Monetary value, in Ergo tokens */
  def R0[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](0)

  /** Mandatory: Guarding script */
  def R1[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](1)

  /** Mandatory: Secondary tokens */
  def R2[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](2)

  /** Mandatory: Reference to transaction and output id where the box was created */
  def R3[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](3)

  // Non-mandatory registers
  def R4[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](4)
  def R5[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](5)
  def R6[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](6)
  def R7[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](7)
  def R8[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](8)
  def R9[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](9)

  def tokens: Col[(Col[Byte], Long)]
  def creationInfo: (Int, Col[Byte])
  @Internal
  override def toString = s"Box(id=$id; value=$value; cost=$cost; size=$dataSize; regs=$registers)"
}

@scalan.Liftable
trait AvlTree extends DslObject {
  def startingDigest: Col[Byte]
  def keyLength: Int
  def valueLengthOpt: Option[Int]
  def maxNumOperations: Option[Int]
  def maxDeletes: Option[Int]
  def cost: Int
  def dataSize: Long
}

@scalan.Liftable
trait Context {
  def builder: SigmaDslBuilder
  def OUTPUTS: Col[Box]
  def INPUTS: Col[Box]
  def HEIGHT: Int
  def SELF: Box
  def LastBlockUtxoRootHash: AvlTree
  def MinerPubKey: Col[Byte]
  def getVar[T](id: Byte)(implicit cT: RType[T]): Option[T]
  def getConstant[T](id: Byte)(implicit cT: RType[T]): T
  def cost: Int
  def dataSize: Long
}

@scalan.Liftable
trait SigmaContract {
  def builder: SigmaDslBuilder

  @NeverInline
  @Reified("T")
  def Collection[T](items: T*)(implicit cT: ClassTag[T]): Col[T] = this.builder.Cols.fromItems[T](items:_*)

  /** !!! all methods should delegate to builder */

  def verifyZK(cond: => SigmaProp): Boolean = this.builder.verifyZK(cond)
  def atLeast(bound: Int, props: Col[SigmaProp]): SigmaProp = this.builder.atLeast(bound, props)

  def allOf(conditions: Col[Boolean]): Boolean = this.builder.allOf(conditions)
  def allZK(conditions: Col[SigmaProp]): SigmaProp = this.builder.allZK(conditions)

  def anyOf(conditions: Col[Boolean]): Boolean = this.builder.anyOf(conditions)
  def anyZK(conditions: Col[SigmaProp]): SigmaProp = this.builder.anyZK(conditions)

  def PubKey(base64String: String): SigmaProp = this.builder.PubKey(base64String)

  def sigmaProp(b: Boolean): SigmaProp = this.builder.sigmaProp(b)

  def blake2b256(bytes: Col[Byte]): Col[Byte] = this.builder.blake2b256(bytes)
  def sha256(bytes: Col[Byte]): Col[Byte] = this.builder.sha256(bytes)

  def byteArrayToBigInt(bytes: Col[Byte]): BigInteger = this.builder.byteArrayToBigInt(bytes)
  def longToByteArray(l: Long): Col[Byte] = this.builder.longToByteArray(l)

  def proveDlog(g: ECPoint): SigmaProp = this.builder.proveDlog(g)
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): SigmaProp = this.builder.proveDHTuple(g, h, u, v)

  def isMember(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Boolean = this.builder.isMember(tree, key, proof)
  def treeLookup(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Option[Col[Byte]] = this.builder.treeLookup(tree, key, proof)
  def treeModifications(tree: AvlTree, operations: Col[Byte], proof: Col[Byte]): Option[Col[Byte]] = this.builder.treeModifications(tree, operations, proof)

  def groupGenerator: ECPoint = this.builder.groupGenerator
  def exponentiate(base: ECPoint, exponent: BigInteger): ECPoint = this.builder.exponentiate(base, exponent)

  @clause def canOpen(ctx: Context): Boolean

  def asFunction: Context => Boolean = (ctx: Context) => this.canOpen(ctx)
}

@scalan.Liftable
trait SigmaDslBuilder extends DslBuilder {
  def Cols: ColBuilder
  def Monoids: MonoidBuilder
  def Costing: CostedBuilder
  def CostModel: CostModel

  def costBoxes(bs: Col[Box]): CostedCol[Box]

  /** Cost of collection with static size elements. */
  def costColWithConstSizedItem[T](xs: Col[T], len: Int, itemSize: Long): CostedCol[T]

  def costOption[T](opt: Option[T], opCost: Int)(implicit cT: RType[T]): CostedOption[T]

  def verifyZK(cond: => SigmaProp): Boolean

  def atLeast(bound: Int, props: Col[SigmaProp]): SigmaProp

  def allOf(conditions: Col[Boolean]): Boolean
  def allZK(conditions: Col[SigmaProp]): SigmaProp

  def anyOf(conditions: Col[Boolean]): Boolean
  def anyZK(conditions: Col[SigmaProp]): SigmaProp

  def PubKey(base64String: String): SigmaProp

  def sigmaProp(b: Boolean): SigmaProp

  def blake2b256(bytes: Col[Byte]): Col[Byte]
  def sha256(bytes: Col[Byte]): Col[Byte]

  def byteArrayToBigInt(bytes: Col[Byte]): BigInteger
  def longToByteArray(l: Long): Col[Byte]

  def proveDlog(g: ECPoint): SigmaProp
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): SigmaProp

  def isMember(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Boolean
  def treeLookup(tree: AvlTree, key: Col[Byte], proof: Col[Byte]): Option[Col[Byte]]
  def treeModifications(tree: AvlTree, operations: Col[Byte], proof: Col[Byte]): Option[Col[Byte]]

  def groupGenerator: ECPoint
  def exponentiate(base: ECPoint, exponent: BigInteger): ECPoint
  @Reified("T")
  def substConstants[T](scriptBytes: Col[Byte], positions: Col[Int], newValues: Col[T])(implicit cT: RType[T]): Col[Byte]
  def decodePoint(encoded: Col[Byte]): ECPoint
}

