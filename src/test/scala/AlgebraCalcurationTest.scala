import org.scalatest.FunSpec

class AlgebraCalcurationTest extends FunSpec {
  describe("AlgebraCalcurationの") {
    describe("addのテスト") {
      it("1 plus 1 == 2") {
        val algebraCalcuration = new AlgebraCalcuration
        assert(algebraCalcuration.add(1, 1) === 2)
      }

      it("2 plus 3 == 5") {
        val algebraCalcuration = new AlgebraCalcuration
       assert(algebraCalcuration.add(5, 5) === 10)
      }
    }
  }
}
