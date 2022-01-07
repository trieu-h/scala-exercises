sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeExercise {
  val tree =
    Branch(
      Branch(Leaf(6), Leaf(9)),
      Branch(Leaf(2), Leaf(5)),
    )

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(v)   => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B =
    tree match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _ + 1)

  def maxViaFold(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  // def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
  //   fold(tree)(v => Leaf(f(v)))(Branch(_, _))
  // this doesn't compile for some reason

  def runSizeTest(): Unit = {
    assert(size(tree) == 7)
  }

  def runMaximumTest(): Unit = {
    assert(maximum(tree) == 9)
  }

  def main(args: Array[String]): Unit = {
    println(size(tree))
  }
}
