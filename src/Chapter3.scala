sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]) : Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*) : List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]) : List[A] = as match {
    case Nil => Nil
    case Cons(_, rest) => rest
  }

  def setHead[A](as: List[A], a: A) : List[A] = as match {
    case Nil => Nil
    case Cons(_, rest) => Cons(a, rest)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(times: Int, l: List[A]): List[A] = {
      if (times == 0 || l == Nil) l
      else loop(times - 1, tail(l))
    }
    loop(n, l)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, rest) =>
      if (f(x)) dropWhile(rest, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, rest) => Cons(x, init(rest))
  }
}
