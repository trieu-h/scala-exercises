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

  def append[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(_ :: _)

  def add1(as: List[Int]): List[Int] = {
    as match {
      case Nil => Nil
      case x :: xs => (x + 1) :: add1(xs)
    }
  }

  def toString(as: List[Double]): List[String] = {
    as match {
      case Nil => Nil
      case x :: xs => x.toString() :: toString(xs)
    }
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs)(f)
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a, acc) => if (f(a)) a :: acc else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((a, acc) => append(f(a), acc))

  def filterViaFlatMap[A](as: List[A])(p: A => Boolean): List[A] = {
    flatMap(as)(a => if (p(a)) List(a) else List())
  }

  def zipWith[A,B](as: List[A], bs: List[A])(f: (A,A) => B): List[B] =
    (as, bs) match {
      case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
      case (_, _) => Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil     => false
      case x :: xs => if (x == sub.head && sup.take(sub.length) == sub)
                      true else hasSubsequence(xs, sub)
    }

  def sum[A](as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product[A](as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, x) => x + 1)

  def runMapTest(): Unit = {
    assert(map(List(1,2))(x => x+1) == List(2, 3))
    assert(map(List())(x => x)      == List())
  }

  def runAdd1Test(): Unit = {
    assert(add1(List(1,2,3)) == List(2,3,4))
    assert(add1(List(1))     == List(2))
  }

  def runAppendTest(): Unit = {
    assert(append(List(1,2), List(3,4)) == List(1,2,3,4))
  }

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

  def runZipWithTest(): Unit = {
    assert(zipWith(List(1,2,3), List(2,3,4))(_ + _) == List(3,5,7))
  }

  def runFilterTest(): Unit = {
    assert(filterViaFlatMap(List(2,1,3))(_ > 1) == List(2,3))
  }

  def main(args: Array[String]): Unit = {
    println(hasSubsequence(List(2,4,3,1), List(4,3)))
  }
}
