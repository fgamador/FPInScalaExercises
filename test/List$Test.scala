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

  test("testInit") {
    assert(List.init(Nil) == Nil)
    assert(List.init(List(1)) == Nil)
    assert(List.init(List(1, 2)) == List(1))
  }

  test("testFoldRight_Sum") {
    val sum = (l: List[Int]) => List.foldRight(l, 0)(_ + _)
    assert(sum(Nil) == 0)
    assert(sum(List(2)) == 2)
    assert(sum(List(1, 2, 3)) == 6)
  }

  test("testFoldRight_List") {
    val copy = (l: List[Int]) => List.foldRight(l, Nil: List[Int])(Cons(_, _))
    assert(copy(Nil) == Nil)
    assert(copy(List(2)) == List(2))
    assert(copy(List(1, 2, 3)) == List(1, 2, 3))
  }

  test("testLength") {
    assert(List.length(Nil) == 0)
    assert(List.length(List(1, 2, 3)) == 3)
  }

  test("testFoldLeft_Sum") {
    val sum = (l: List[Int]) => List.foldLeft(l, 0)(_ + _)
    assert(sum(Nil) == 0)
    assert(sum(List(2)) == 2)
    assert(sum(List(1, 2, 3)) == 6)
  }

  test("testFoldLeft_List") {
    val reverse = (l: List[Int]) => List.foldLeft(l, Nil: List[Int])((t, h) => Cons(h, t))
    assert(reverse(Nil) == Nil)
    assert(reverse(List(2)) == List(2))
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("testSum_FoldLeft") {
    assert(List.sum_fl(Nil) == 0)
    assert(List.sum_fl(List(2)) == 2)
    assert(List.sum_fl(List(1, 2, 3)) == 6)
  }

  test("testProduct_FoldLeft") {
    assert(List.product_fl(Nil) == 1)
    assert(List.product_fl(List(2)) == 2)
    assert(List.product_fl(List(2, 3)) == 6)
  }

  test("testLength_FoldLeft") {
    assert(List.length_fl(Nil) == 0)
    assert(List.length_fl(List(1, 2, 3)) == 3)
  }

  test("testReverse_FoldLeft") {
    assert(List.reverse_fl(Nil) == Nil)
    assert(List.reverse_fl(List(2)) == List(2))
    assert(List.reverse_fl(List(1, 2, 3)) == List(3, 2, 1))
  }
}
