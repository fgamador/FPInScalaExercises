import org.scalatest.FunSuite

class Chapter2$Test extends FunSuite {
  test("testFib") {
    assert(Chapter2.fib(0) == 0)
    assert(Chapter2.fib(1) == 1)
    assert(Chapter2.fib(2) == 1)
    assert(Chapter2.fib(3) == 2)
    assert(Chapter2.fib(4) == 3)
    assert(Chapter2.fib(5) == 5)
    assert(Chapter2.fib(6) == 8)
    assert(Chapter2.fib(7) == 13)
  }

  test("testIsSorted") {
  }
}
