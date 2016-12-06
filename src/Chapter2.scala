object Chapter2 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(times: Int, prevprev: Int, prev: Int): Int = {
      val next = prevprev + prev
      if (times <= 1) next
      else loop(times - 1, prev, next)
    }

    if (n <= 1) n
    else loop(n - 1, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    false
  }
}
