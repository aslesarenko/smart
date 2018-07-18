package special.sigma.wrappers {
  import scalan._

  trait WrappersSpec extends Base { self: SigmaLibrary =>
    import WArray._;
    import WrapSpec._;
    import WSigmaPredef._;
    import WECPoint._;
    abstract class ECPointWrapSpec extends WrapSpec {
      def getEncoded[A](g: Rep[WECPoint]): Rep[WArray[Byte]] = g.getEncoded(toRep(true.asInstanceOf[Boolean]))
    };
    abstract class SigmaPredefWrapSpec extends WrapSpec {
      def cost(v: Rep[Any]): Rep[Int] = RWSigmaPredef.cost(v)
    };
    trait ECPointWrapSpecCompanion;
    trait SigmaPredefWrapSpecCompanion
  }
}