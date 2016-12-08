import org.scalatest.FunSuite

class List$Test extends FunSuite {
  test("testTail") {
    assert(List.tail(Nil) == Nil)
    assert(List.tail(List("A")) == Nil)
    assert(List.tail(List("A", "B", "C")) == List("B", "C"))
  }
}
