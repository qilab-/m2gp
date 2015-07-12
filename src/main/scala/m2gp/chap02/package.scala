package m2gp

import scala.annotation.tailrec

package object chap02 {

  def multiply0(n: Int, a: Int): Int = {
    require(n > 0 && a > 0)
    if (n == 1) a
    else multiply0(n - 1, a) + a
  }

  def half(n: Int): Int = n >> 1
  def odd(n: Int): Boolean = n % 2 == 1
  def even(n: Int): Boolean = !odd(n)

  // This algorithm is called Egyptian multiplication or Russian Peasant Algorithm
  def multiply1(n: Int, a: Int): Int = {
    require(n > 0 && a > 0)
    n match {
      case 1 => a
      case m =>
        if (even(n))
          multiply1(half(n), a + a)
        else
          multiply1(half(n), a + a) + a
    }
  }

  def multiply_by_15(a: Int): Int = {
    val b = (a + a) + a
    val c = b + b
    (c + c) + b
  }

  // r + n * a
  def mult_acc0(r: Int, n: Int, a: Int): Int = {
    n match {
      case 1 => r + a
      case m =>
        if (odd(m))
          mult_acc0(r + a, half(n), a + a)
        else
          mult_acc0(r, half(n), a + a)
    }
  }
  @tailrec
  def mult_acc1(r: Int, n: Int, a: Int): Int = {
    n match {
      case 1 => r + a
      case m =>
        val s = if (odd(n)) r + a else r
        mult_acc1(s, half(n), a + a)
    }
  }
  @tailrec
  def mult_acc2(r: Int, n: Int, a: Int): Int = {
    val s = if (odd(n)) {
      if (n == 1) return r + a
      r + a
    } else {
      r
    }
    mult_acc2(s, half(n), a + a)
  }

  def multiply2(n: Int, a: Int): Int = {
    if (n == 1) a else mult_acc2(a, n - 1, a)
  }

  def multiply4(n: Int, a: Int): Int = {
    val (nn, an) = Stream.iterate((n, a)) { case (m, b) =>
      (half(m), b + b)
    }.takeWhile(t => even(t._1)).lastOption.fold((n, a)) { case (m, b) =>
      (half(m), b + b)
    }
    if (nn == 1)
      an
    else // even(nn - a) => n - 1 != 1
      mult_acc2(an, half(nn - 1), an + an)
  }

}
