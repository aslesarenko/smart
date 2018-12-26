package special.sigma {
  import scalan._

  trait CostedObjects extends Base { self: SigmaLibrary =>
    import AnyValue._;
    import AvlTree._;
    import Box._;
    import Col._;
    import Context._;
    import Costed._;
    import CostedAvlTree._;
    import CostedBox._;
    import CostedBuilder._;
    import CostedCol._;
    import CostedOption._;
    import CostedSigmaObject._;
    import SigmaDslBuilder._;
    trait CostedSigmaObject[Val] extends Costed[Val] {
      implicit def eVal: Elem[Val];
      def dsl: Rep[SigmaDslBuilder];
      def builder: Rep[CostedBuilder] = CostedSigmaObject.this.dsl.Costing
    };
    trait CostedContext extends CostedSigmaObject[Context] {
      def OUTPUTS: Rep[CostedCol[Box]];
      def INPUTS: Rep[CostedCol[Box]];
      def HEIGHT: Rep[Costed[Int]];
      def SELF: Rep[CostedBox];
      def LastBlockUtxoRootHash: Rep[CostedAvlTree];
      def MinerPubKey: Rep[CostedCol[Byte]];
      def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[CostedOption[T]];
      def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[Costed[T]]
    };
    trait CostedBox extends CostedSigmaObject[Box] {
      def id: Rep[CostedCol[Byte]];
      def valueCosted: Rep[Costed[Long]];
      def bytes: Rep[CostedCol[Byte]];
      def bytesWithoutRef: Rep[CostedCol[Byte]];
      def propositionBytes: Rep[CostedCol[Byte]];
      def registers: Rep[CostedCol[AnyValue]];
      def getReg[T](id: Rep[Int])(implicit cT: Elem[T]): Rep[CostedOption[T]];
      def creationInfo: Rep[Costed[scala.Tuple2[Int, Col[Byte]]]]
    };
    trait CostedAvlTree extends CostedSigmaObject[AvlTree] {
      def startingDigest: Rep[CostedCol[Byte]];
      def keyLength: Rep[Costed[Int]];
      def valueLengthOpt: Rep[CostedOption[Int]];
      def maxNumOperations: Rep[CostedOption[Int]];
      def maxDeletes: Rep[CostedOption[Int]]
    };
    trait CostedSigmaObjectCompanion;
    trait CostedContextCompanion;
    trait CostedBoxCompanion;
    trait CostedAvlTreeCompanion
  }
}