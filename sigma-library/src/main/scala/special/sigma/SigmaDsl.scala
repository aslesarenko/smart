package special.sigma {
  import scalan.OverloadHack.Overloaded1   // manual fix
  import scalan._

  trait SigmaDsl extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import Col._;
    import ColBuilder._;
    import Context._;
    import CostModel._;
    import CostedBuilder._;
    import CostedCol._;
    import CostedOption._;
    import DslBuilder._;
    import DslObject._;
    import MonoidBuilder._;
    import SigmaContract._;
    import SigmaDslBuilder._;
    import SigmaProp._;
    import WBigInteger._;
    import WECPoint._;
    import WOption._;
    @Liftable trait CostModel extends Def[CostModel] {
      def AccessBox: Rep[Int];
      def AccessAvlTree: Rep[Int];
      def GetVar: Rep[Int];
      def DeserializeVar: Rep[Int];
      def GetRegister: Rep[Int];
      def DeserializeRegister: Rep[Int];
      def SelectField: Rep[Int];
      def CollectionConst: Rep[Int];
      def AccessKiloByteOfData: Rep[Int];
      @Reified(value = "T") def dataSize[T](x: Rep[T])(implicit cT: Elem[T]): Rep[Long];
      def PubKeySize: Rep[Long] = toRep(32L.asInstanceOf[Long])
    };
    trait DslBuilder extends Def[DslBuilder];
    trait DslObject { // manual fix
      def builder: Rep[SigmaDslBuilder]
    };
    // manual fix (Def)
    @Liftable trait SigmaProp extends Def[SigmaProp] with DslObject {
      def isValid: Rep[Boolean];
      def propBytes: Rep[Col[Byte]];
      @OverloadId(value = "and_sigma") def &&(other: Rep[SigmaProp]): Rep[SigmaProp];
      // manual fix
      @OverloadId(value = "and_bool") def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp];
      @OverloadId(value = "or_sigma") def ||(other: Rep[SigmaProp]): Rep[SigmaProp];
      // manual fix
      @OverloadId(value = "or_bool") def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp];
      def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp];
      def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp]
    };
    @Liftable trait AnyValue extends Def[AnyValue] {
      def dataSize: Rep[Long]
    };
    // manual fix (Def)
    @Liftable trait Box extends Def[Box] with DslObject {
      def id: Rep[Col[Byte]];
      def value: Rep[Long];
      def bytes: Rep[Col[Byte]];
      def bytesWithoutRef: Rep[Col[Byte]];
      def propositionBytes: Rep[Col[Byte]];
      def cost: Rep[Int];
      def dataSize: Rep[Long];
      def registers: Rep[Col[AnyValue]];
      def getReg[T](i: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]];
      def R0[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(0.asInstanceOf[Int]));
      def R1[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(1.asInstanceOf[Int]));
      def R2[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(2.asInstanceOf[Int]));
      def R3[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(3.asInstanceOf[Int]));
      def R4[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(4.asInstanceOf[Int]));
      def R5[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(5.asInstanceOf[Int]));
      def R6[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(6.asInstanceOf[Int]));
      def R7[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(7.asInstanceOf[Int]));
      def R8[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(8.asInstanceOf[Int]));
      def R9[T](implicit cT: Elem[T]): Rep[WOption[T]] = this.getReg[T](toRep(9.asInstanceOf[Int]));
      def tokens: Rep[Col[scala.Tuple2[Col[Byte], Long]]];
      def creationInfo: Rep[scala.Tuple2[Long, Col[Byte]]]
    };
    // manual fix (Def)
    @Liftable trait AvlTree extends Def[AvlTree] with DslObject {
      def startingDigest: Rep[Col[Byte]];
      def keyLength: Rep[Int];
      def valueLengthOpt: Rep[WOption[Int]];
      def maxNumOperations: Rep[WOption[Int]];
      def maxDeletes: Rep[WOption[Int]];
      def cost: Rep[Int];
      def dataSize: Rep[Long]
    };
    @Liftable trait Context extends Def[Context] {
      def builder: Rep[SigmaDslBuilder];
      def OUTPUTS: Rep[Col[Box]];
      def INPUTS: Rep[Col[Box]];
      def HEIGHT: Rep[Int];
      def SELF: Rep[Box];
      def LastBlockUtxoRootHash: Rep[AvlTree];
      def MinerPubKey: Rep[Col[Byte]];
      def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]];
      def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[T];
      def cost: Rep[Int];
      def dataSize: Rep[Long]
    };
    @Liftable trait SigmaContract extends Def[SigmaContract] {
      def builder: Rep[SigmaDslBuilder];
      @NeverInline @Reified(value = "T") def Collection[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Col[T]] = delayInvoke;
      def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean] = this.builder.verifyZK(cond);
      def atLeast(bound: Rep[Int], props: Rep[Col[SigmaProp]]): Rep[SigmaProp] = this.builder.atLeast(bound, props);
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = this.builder.allOf(conditions);
      def allZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = this.builder.allZK(conditions);
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean] = this.builder.anyOf(conditions);
      def anyZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp] = this.builder.anyZK(conditions);
      def PubKey(base64String: Rep[String]): Rep[SigmaProp] = this.builder.PubKey(base64String);
      def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = this.builder.sigmaProp(b);
      def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = this.builder.blake2b256(bytes);
      def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]] = this.builder.sha256(bytes);
      def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger] = this.builder.byteArrayToBigInt(bytes);
      def longToByteArray(l: Rep[Long]): Rep[Col[Byte]] = this.builder.longToByteArray(l);
      def proveDlog(g: Rep[WECPoint]): Rep[SigmaProp] = this.builder.proveDlog(g);
      def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[SigmaProp] = this.builder.proveDHTuple(g, h, u, v);
      def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean] = this.builder.isMember(tree, key, proof);
      def treeLookup(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = this.builder.treeLookup(tree, key, proof);
      def treeModifications(tree: Rep[AvlTree], operations: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]] = this.builder.treeModifications(tree, operations, proof);
      def groupGenerator: Rep[WECPoint] = this.builder.groupGenerator;
      def exponentiate(base: Rep[WECPoint], exponent: Rep[WBigInteger]): Rep[WECPoint] = this.builder.exponentiate(base, exponent);
      @clause def canOpen(ctx: Rep[Context]): Rep[Boolean];
      def asFunction: Rep[scala.Function1[Context, Boolean]] = fun(((ctx: Rep[Context]) => this.canOpen(ctx)))
    };
    // manual fix (Def)
    @Liftable trait SigmaDslBuilder extends Def[SigmaDslBuilder] with DslBuilder {
      def Cols: Rep[ColBuilder];
      def Monoids: Rep[MonoidBuilder];
      def Costing: Rep[CostedBuilder];
      def CostModel: Rep[CostModel];
      def costBoxes(bs: Rep[Col[Box]]): Rep[CostedCol[Box]];
      def costColWithConstSizedItem[T](xs: Rep[Col[T]], len: Rep[Int], itemSize: Rep[Long]): Rep[CostedCol[T]];
      def costOption[T](opt: Rep[WOption[T]], opCost: Rep[Int]): Rep[CostedOption[T]];
      def verifyZK(cond: Rep[Thunk[SigmaProp]]): Rep[Boolean];
      def atLeast(bound: Rep[Int], props: Rep[Col[SigmaProp]]): Rep[SigmaProp];
      def allOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def allZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp];
      def anyOf(conditions: Rep[Col[Boolean]]): Rep[Boolean];
      def anyZK(conditions: Rep[Col[SigmaProp]]): Rep[SigmaProp];
      def PubKey(base64String: Rep[String]): Rep[SigmaProp];
      def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp];
      def blake2b256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]];
      def sha256(bytes: Rep[Col[Byte]]): Rep[Col[Byte]];
      def byteArrayToBigInt(bytes: Rep[Col[Byte]]): Rep[WBigInteger];
      def longToByteArray(l: Rep[Long]): Rep[Col[Byte]];
      def proveDlog(g: Rep[WECPoint]): Rep[SigmaProp];
      def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[SigmaProp];
      def isMember(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[Boolean];
      def treeLookup(tree: Rep[AvlTree], key: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]];
      def treeModifications(tree: Rep[AvlTree], operations: Rep[Col[Byte]], proof: Rep[Col[Byte]]): Rep[WOption[Col[Byte]]];
      def groupGenerator: Rep[WECPoint];
      def exponentiate(base: Rep[WECPoint], exponent: Rep[WBigInteger]): Rep[WECPoint];
      @Reified(value = "T") def substConstants[T](scriptBytes: Rep[Col[Byte]], positions: Rep[Col[Int]], newValues: Rep[Col[T]])(implicit cT: Elem[T]): Rep[Col[Byte]];
      def decodePoint(encoded: Rep[Col[Byte]]): Rep[WECPoint]
    };
    trait CostModelCompanion;
    trait DslBuilderCompanion;
    trait DslObjectCompanion;
    trait SigmaPropCompanion;
    trait AnyValueCompanion;
    trait BoxCompanion;
    trait AvlTreeCompanion;
    trait ContextCompanion;
    trait SigmaContractCompanion;
    trait SigmaDslBuilderCompanion
  }
}