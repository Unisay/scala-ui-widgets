import utest._

class TTest extends TestSuite {
  override def tests = this {

    'test {
      println("test")
    }

  }
}
