object Exercise {
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil      => Nil
      case _ :: xs  => xs
    }

  def setHead[A](l: List[A], y: A): List[A] =
    l match {
      case Nil      => Nil
      case _ :: xs  => y :: xs
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    if(l.length < n) sys.error("Taking more elements than list's length")
    l match {
      case _ :: xs if n > 0  => drop(xs, n - 1)
      case _       if n == 0 => l
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case x :: xs  if f(x) => dropWhile(xs)(f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil           => Nil
      case _ :: Nil      => Nil
      case x :: xs       => x :: init(xs)
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum[A](as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product[A](as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, x) => x + 1)

  def runSumTest(): Unit = {
    assert(sum(List(1,2,3)) == 6)
    assert(sum(List(0))     == 0)
  }

  def runProductTest(): Unit = {
    assert(sum(List(3,4,5)) == 60)
    assert(sum(List(0))     == 0)
  }

  def runDropTest(): Unit = {
    assert(drop(List(1,2,3), 1)     == List(2,3))
    assert(drop(List(2,3,4,5,6), 3) == List(5,6))
  }

  def runDropWhileTest(): Unit = {
    assert(dropWhile(List(0,0,0,1))(x => x < 1) == List(1))
    assert(dropWhile(List(1))(x => x < 1)       == List(1))
  }

  def runTailTest(): Unit = {
    assert(List(2,3) == tail((List(1,2,3))))
    assert(Nil       == tail(List(1)))
  }

  def runSetHeadTest(): Unit = {
    assert(setHead(List(1,2,3), 4) == List(4,2,3))
    assert(setHead(List(6), 9)     == List(9))
  }

  def main(args: Array[String]): Unit = {
    runTailTest();
    runSetHeadTest();
    runDropTest();
    runDropWhileTest();
    runSumTest();
  }
}
