import org.scalatest.FunSuite
import Chapter2._

class Chapter2$Test extends FunSuite {
  test("testFib") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
    assert(fib(6) == 8)
    assert(fib(7) == 13)
  }

  test("testIsSorted") {
    val lessThan = (lhs: Int, rhs: Int) => lhs < rhs
    assert(isSorted(Array(), lessThan))
    assert(isSorted(Array(1), lessThan))
    assert(isSorted(Array(1, 2, 3), lessThan))
    assert(!isSorted(Array(1, 3, 2), lessThan))
  }
}
