import org.scalatest.FunSuite

class List$Test extends FunSuite {
  test("testTail") {
    assert(List.tail(Nil) == Nil)
    assert(List.tail(List("A")) == Nil)
    assert(List.tail(List("A", "B", "C")) == List("B", "C"))
  }

  test("testSetHead") {
    assert(List.setHead(Nil, "A") == Nil)
    assert(List.setHead(List("A"), "B") == List("B"))
    assert(List.setHead(List("A", "B"), "D") == List("D", "B"))
  }

  test("testDrop") {
    assert(List.drop(Nil, 0) == Nil)
    assert(List.drop(Nil, 1) == Nil)
    assert(List.drop(List("A", "B"), 0) == List("A", "B"))
    assert(List.drop(List("A", "B"), 1) == List("B"))
    assert(List.drop(List("A", "B"), 2) == Nil)
    assert(List.drop(List("A", "B"), 3) == Nil)
  }

  test("testDropWhile") {
    val f = (i: Int) => i < 3
    assert(List.dropWhile(Nil, f) == Nil)
    assert(List.dropWhile(List(3), f) == List(3))
    assert(List.dropWhile(List(1), f) == Nil)
    assert(List.dropWhile(List(1, 2, 3), f) == List(3))
  }
}
