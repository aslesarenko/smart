package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslCostedDefs extends scalan.Scalan with SigmaDslCosted {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import AnyValue._
import AvlTree._
import Box._
import CCostedAvlTree._
import CCostedBox._
import CCostedCol._
import CCostedPrim._
import Col._
import Context._
import Costed._
import CostedAvlTree._
import CostedBox._
import CostedCol._
import CostedContext._
import CostedOption._
import SigmaDslBuilder._
import TestSigmaDslBuilder._
import CCostedContext._

object CCostedContext extends EntityObject("CCostedContext") {
  case class CCostedContextCtor
      (override val ctx: Rep[Context])
    extends CCostedContext(ctx) with Def[CCostedContext] {
    override lazy val eVal: Elem[Context] = implicitly[Elem[Context]]
    lazy val selfType = element[CCostedContext]
    private val thisClass = classOf[CCostedContext]

    override def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[Costed[T]] = {
      asRep[Costed[T]](mkMethodCall(self,
        thisClass.getMethod("getConstant", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, false, element[Costed[T]]))
    }
  }
  // elem for concrete class
  class CCostedContextElem(val iso: Iso[CCostedContextData, CCostedContext])
    extends CostedContextElem[CCostedContext]
    with ConcreteElem[CCostedContextData, CCostedContext] {
    override lazy val parent: Option[Elem[_]] = Some(costedContextElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertCostedContext(x: Rep[CostedContext]) = // Converter is not generated by meta
!!!("Cannot convert from CostedContext to CCostedContext: missing fields List(ctx)")
    override def getDefaultRep = RCCostedContext(element[Context].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[CCostedContext]
    }
  }

  // state representation type
  type CCostedContextData = Context

  // 3) Iso for concrete class
  class CCostedContextIso
    extends EntityIso[CCostedContextData, CCostedContext] with Def[CCostedContextIso] {
    private lazy val _safeFrom = fun { p: Rep[CCostedContext] => p.ctx }
    override def from(p: Rep[CCostedContext]) =
      tryConvert[CCostedContext, Context](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Context]) = {
      val ctx = p
      RCCostedContext(ctx)
    }
    lazy val eFrom = element[Context]
    lazy val eTo = new CCostedContextElem(self)
    lazy val selfType = new CCostedContextIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CCostedContextIsoElem() extends Elem[CCostedContextIso] {
    def getDefaultRep = reifyObject(new CCostedContextIso())
    lazy val tag = {
      weakTypeTag[CCostedContextIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CCostedContextCompanionCtor extends CompanionDef[CCostedContextCompanionCtor] with CCostedContextCompanion {
    def selfType = CCostedContextCompanionElem
    override def toString = "CCostedContextCompanion"

    @scalan.OverloadId("fromFields")
    def apply(ctx: Rep[Context]): Rep[CCostedContext] =
      mkCCostedContext(ctx)

    def unapply(p: Rep[CostedContext]) = unmkCCostedContext(p)
  }
  lazy val CCostedContextRep: Rep[CCostedContextCompanionCtor] = new CCostedContextCompanionCtor
  lazy val RCCostedContext: CCostedContextCompanionCtor = proxyCCostedContextCompanion(CCostedContextRep)
  implicit def proxyCCostedContextCompanion(p: Rep[CCostedContextCompanionCtor]): CCostedContextCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedContextCompanionCtor])
      p.rhs.asInstanceOf[CCostedContextCompanionCtor]
    else
      proxyOps[CCostedContextCompanionCtor](p)
  }

  implicit case object CCostedContextCompanionElem extends CompanionElem[CCostedContextCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedContextCompanionCtor]
    protected def getDefaultRep = CCostedContextRep
  }

  implicit def proxyCCostedContext(p: Rep[CCostedContext]): CCostedContext =
    proxyOps[CCostedContext](p)

  implicit class ExtendedCCostedContext(p: Rep[CCostedContext]) {
    def toData: Rep[CCostedContextData] = {
      isoCCostedContext.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedContext: Iso[CCostedContextData, CCostedContext] =
    reifyObject(new CCostedContextIso())

  def mkCCostedContext
    (ctx: Rep[Context]): Rep[CCostedContext] = {
    new CCostedContextCtor(ctx)
  }
  def unmkCCostedContext(p: Rep[CostedContext]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedContextElem @unchecked =>
      Some((asRep[CCostedContext](p).ctx))
    case _ =>
      None
  }

    object CCostedContextMethods {
    object dsl {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "dsl" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "OUTPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "INPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "HEIGHT" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "SELF" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object LastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "LastBlockUtxoRootHash" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object MinerPubKey {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "MinerPubKey" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "getVar" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getConstant {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "getConstant" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedContextElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedContextCompanionMethods {
  }
} // of object CCostedContext
  registerEntityObject("CCostedContext", CCostedContext)

object CCostedBox extends EntityObject("CCostedBox") {
  case class CCostedBoxCtor
      (override val box: Rep[Box], override val cost: Rep[Int])
    extends CCostedBox(box, cost) with Def[CCostedBox] {
    override lazy val eVal: Elem[Box] = implicitly[Elem[Box]]
    lazy val selfType = element[CCostedBox]
    private val thisClass = classOf[CCostedBox]

    override def creationInfo: Rep[Costed[(Int, Col[Byte])]] = {
      asRep[Costed[(Int, Col[Byte])]](mkMethodCall(self,
        thisClass.getMethod("creationInfo"),
        List(),
        true, false, element[Costed[(Int, Col[Byte])]]))
    }
  }
  // elem for concrete class
  class CCostedBoxElem(val iso: Iso[CCostedBoxData, CCostedBox])
    extends CostedBoxElem[CCostedBox]
    with ConcreteElem[CCostedBoxData, CCostedBox] {
    override lazy val parent: Option[Elem[_]] = Some(costedBoxElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertCostedBox(x: Rep[CostedBox]) = // Converter is not generated by meta
!!!("Cannot convert from CostedBox to CCostedBox: missing fields List(box)")
    override def getDefaultRep = RCCostedBox(element[Box].defaultRepValue, 0)
    override lazy val tag = {
      weakTypeTag[CCostedBox]
    }
  }

  // state representation type
  type CCostedBoxData = (Box, Int)

  // 3) Iso for concrete class
  class CCostedBoxIso
    extends EntityIso[CCostedBoxData, CCostedBox] with Def[CCostedBoxIso] {
    private lazy val _safeFrom = fun { p: Rep[CCostedBox] => (p.box, p.cost) }
    override def from(p: Rep[CCostedBox]) =
      tryConvert[CCostedBox, (Box, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Box, Int)]) = {
      val Pair(box, cost) = p
      RCCostedBox(box, cost)
    }
    lazy val eFrom = pairElement(element[Box], element[Int])
    lazy val eTo = new CCostedBoxElem(self)
    lazy val selfType = new CCostedBoxIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CCostedBoxIsoElem() extends Elem[CCostedBoxIso] {
    def getDefaultRep = reifyObject(new CCostedBoxIso())
    lazy val tag = {
      weakTypeTag[CCostedBoxIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CCostedBoxCompanionCtor extends CompanionDef[CCostedBoxCompanionCtor] with CCostedBoxCompanion {
    def selfType = CCostedBoxCompanionElem
    override def toString = "CCostedBoxCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CCostedBoxData]): Rep[CCostedBox] = {
      isoCCostedBox.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(box: Rep[Box], cost: Rep[Int]): Rep[CCostedBox] =
      mkCCostedBox(box, cost)

    def unapply(p: Rep[CostedBox]) = unmkCCostedBox(p)
  }
  lazy val CCostedBoxRep: Rep[CCostedBoxCompanionCtor] = new CCostedBoxCompanionCtor
  lazy val RCCostedBox: CCostedBoxCompanionCtor = proxyCCostedBoxCompanion(CCostedBoxRep)
  implicit def proxyCCostedBoxCompanion(p: Rep[CCostedBoxCompanionCtor]): CCostedBoxCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedBoxCompanionCtor])
      p.rhs.asInstanceOf[CCostedBoxCompanionCtor]
    else
      proxyOps[CCostedBoxCompanionCtor](p)
  }

  implicit case object CCostedBoxCompanionElem extends CompanionElem[CCostedBoxCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedBoxCompanionCtor]
    protected def getDefaultRep = CCostedBoxRep
  }

  implicit def proxyCCostedBox(p: Rep[CCostedBox]): CCostedBox =
    proxyOps[CCostedBox](p)

  implicit class ExtendedCCostedBox(p: Rep[CCostedBox]) {
    def toData: Rep[CCostedBoxData] = {
      isoCCostedBox.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedBox: Iso[CCostedBoxData, CCostedBox] =
    reifyObject(new CCostedBoxIso())

  def mkCCostedBox
    (box: Rep[Box], cost: Rep[Int]): Rep[CCostedBox] = {
    new CCostedBoxCtor(box, cost)
  }
  def unmkCCostedBox(p: Rep[CostedBox]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedBoxElem @unchecked =>
      Some((asRep[CCostedBox](p).box, asRep[CCostedBox](p).cost))
    case _ =>
      None
  }

    object CCostedBoxMethods {
    object dsl {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "dsl" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object id {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "id" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueCosted {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "valueCosted" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytes {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "bytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object bytesWithoutRef {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "bytesWithoutRef" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object propositionBytes {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "propositionBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object registers {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "registers" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBox], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "getReg" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBox], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBox], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object creationInfo {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "creationInfo" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBoxElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedBoxCompanionMethods {
  }
} // of object CCostedBox
  registerEntityObject("CCostedBox", CCostedBox)

object CCostedAvlTree extends EntityObject("CCostedAvlTree") {
  case class CCostedAvlTreeCtor
      (override val tree: Rep[AvlTree], override val cost: Rep[Int])
    extends CCostedAvlTree(tree, cost) with Def[CCostedAvlTree] {
    override lazy val eVal: Elem[AvlTree] = implicitly[Elem[AvlTree]]
    lazy val selfType = element[CCostedAvlTree]
  }
  // elem for concrete class
  class CCostedAvlTreeElem(val iso: Iso[CCostedAvlTreeData, CCostedAvlTree])
    extends CostedAvlTreeElem[CCostedAvlTree]
    with ConcreteElem[CCostedAvlTreeData, CCostedAvlTree] {
    override lazy val parent: Option[Elem[_]] = Some(costedAvlTreeElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertCostedAvlTree(x: Rep[CostedAvlTree]) = // Converter is not generated by meta
!!!("Cannot convert from CostedAvlTree to CCostedAvlTree: missing fields List(tree)")
    override def getDefaultRep = RCCostedAvlTree(element[AvlTree].defaultRepValue, 0)
    override lazy val tag = {
      weakTypeTag[CCostedAvlTree]
    }
  }

  // state representation type
  type CCostedAvlTreeData = (AvlTree, Int)

  // 3) Iso for concrete class
  class CCostedAvlTreeIso
    extends EntityIso[CCostedAvlTreeData, CCostedAvlTree] with Def[CCostedAvlTreeIso] {
    private lazy val _safeFrom = fun { p: Rep[CCostedAvlTree] => (p.tree, p.cost) }
    override def from(p: Rep[CCostedAvlTree]) =
      tryConvert[CCostedAvlTree, (AvlTree, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(AvlTree, Int)]) = {
      val Pair(tree, cost) = p
      RCCostedAvlTree(tree, cost)
    }
    lazy val eFrom = pairElement(element[AvlTree], element[Int])
    lazy val eTo = new CCostedAvlTreeElem(self)
    lazy val selfType = new CCostedAvlTreeIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CCostedAvlTreeIsoElem() extends Elem[CCostedAvlTreeIso] {
    def getDefaultRep = reifyObject(new CCostedAvlTreeIso())
    lazy val tag = {
      weakTypeTag[CCostedAvlTreeIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CCostedAvlTreeCompanionCtor extends CompanionDef[CCostedAvlTreeCompanionCtor] with CCostedAvlTreeCompanion {
    def selfType = CCostedAvlTreeCompanionElem
    override def toString = "CCostedAvlTreeCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CCostedAvlTreeData]): Rep[CCostedAvlTree] = {
      isoCCostedAvlTree.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(tree: Rep[AvlTree], cost: Rep[Int]): Rep[CCostedAvlTree] =
      mkCCostedAvlTree(tree, cost)

    def unapply(p: Rep[CostedAvlTree]) = unmkCCostedAvlTree(p)
  }
  lazy val CCostedAvlTreeRep: Rep[CCostedAvlTreeCompanionCtor] = new CCostedAvlTreeCompanionCtor
  lazy val RCCostedAvlTree: CCostedAvlTreeCompanionCtor = proxyCCostedAvlTreeCompanion(CCostedAvlTreeRep)
  implicit def proxyCCostedAvlTreeCompanion(p: Rep[CCostedAvlTreeCompanionCtor]): CCostedAvlTreeCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedAvlTreeCompanionCtor])
      p.rhs.asInstanceOf[CCostedAvlTreeCompanionCtor]
    else
      proxyOps[CCostedAvlTreeCompanionCtor](p)
  }

  implicit case object CCostedAvlTreeCompanionElem extends CompanionElem[CCostedAvlTreeCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedAvlTreeCompanionCtor]
    protected def getDefaultRep = CCostedAvlTreeRep
  }

  implicit def proxyCCostedAvlTree(p: Rep[CCostedAvlTree]): CCostedAvlTree =
    proxyOps[CCostedAvlTree](p)

  implicit class ExtendedCCostedAvlTree(p: Rep[CCostedAvlTree]) {
    def toData: Rep[CCostedAvlTreeData] = {
      isoCCostedAvlTree.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedAvlTree: Iso[CCostedAvlTreeData, CCostedAvlTree] =
    reifyObject(new CCostedAvlTreeIso())

  def mkCCostedAvlTree
    (tree: Rep[AvlTree], cost: Rep[Int]): Rep[CCostedAvlTree] = {
    new CCostedAvlTreeCtor(tree, cost)
  }
  def unmkCCostedAvlTree(p: Rep[CostedAvlTree]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedAvlTreeElem @unchecked =>
      Some((asRep[CCostedAvlTree](p).tree, asRep[CCostedAvlTree](p).cost))
    case _ =>
      None
  }

    object CCostedAvlTreeMethods {
    object dsl {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "dsl" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object startingDigest {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "startingDigest" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object keyLength {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "keyLength" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valueLengthOpt {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "valueLengthOpt" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object maxNumOperations {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "maxNumOperations" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object maxDeletes {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "maxDeletes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedAvlTreeElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedAvlTreeCompanionMethods {
  }
} // of object CCostedAvlTree
  registerEntityObject("CCostedAvlTree", CCostedAvlTree)

  registerModule(SigmaDslCostedModule)
}

object SigmaDslCostedModule extends scalan.ModuleInfo("special.sigma", "SigmaDslCosted")
}

trait SigmaDslCostedModule extends special.sigma.impl.SigmaDslCostedDefs {self: SigmaLibrary =>}
