object Excersise {
  def fib(n: Int): Int = {
    assert(n > 0)
    @annotation.tailrec
    def fib_(n: Int, sum: Int, prev: Int): Int = {
      if (n > 0) fib_(n - 1, sum + prev, sum)
      else sum
    }

    fib_(n - 1, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val length =
      (as zip as.drop(1))
      .map{ p =>
        val (n1, n2) = p
        ordered(n1, n2)
      }
      .filter(_ == false)
      .length

    !(length > 0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(fib(2))
    println(isSorted(Array(2,1,3), (x: Int, y: Int) => x - y < 0))
  }
}
