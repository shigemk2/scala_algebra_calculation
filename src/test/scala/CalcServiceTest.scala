import org.scalatest.FunSpec

class CalcServiceTest extends FunSpec {
  describe("CalcServiceの") {
    describe("addのテスト") {
      it("1 plus 1 == 2") {
        val calcService = new CalcService
        assert(calcService.add(1, 1) === 2)
      }

      it("5 plus 5 == 10") {
        val calcService = new CalcService
       assert(calcService.add(5, 5) === 10)
      }
    }

    describe("multiplyのテスト") {
      it("1 multiply 1 == 1") {
        val calcService = new CalcService
        assert(calcService.multiply(1, 1) === 1)
      }

      it("5 multiply 5 == 25") {
        val calcService = new CalcService
        assert(calcService.multiply(5, 5) === 25)
      }
    }
  }
}
