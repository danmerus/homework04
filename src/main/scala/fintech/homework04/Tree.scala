package fintech.homework04

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    def recursiveFold( tree: Tree[A]): B = {
      tree match {
        case Leaf(value) => f(value)
        case Branch(left, right) => g(recursiveFold(left), recursiveFold(right))
        }
      }
    recursiveFold(t)
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = {
    fold(t)((_: A) => 1)((x:Int, y:Int) => x+y)
  }

  def max(t: Tree[Int]): Int = {
    fold(t)((x: Int) => x)((x: Int, y: Int) => Math.max(x,y))
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((x, y) => Math.max(x, y) + 1) - 1
  }

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t) (x => Leaf(f(x)): Tree[B]) ((left, right) => Branch(left, right))
  }

}