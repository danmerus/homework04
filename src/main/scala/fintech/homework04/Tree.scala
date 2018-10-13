package fintech.homework04

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    var acc: B = null.asInstanceOf[B]
    def recursiveFold( tree: Tree[A]){
      tree match {
        case Leaf(value) =>
          acc = g(acc, f(value))
        case Branch(left, right) =>
          recursiveFold(left)
          recursiveFold(right)
        }
      }
    recursiveFold(t)
    acc
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = {
    fold[A, Int](t)((_: A) => 1)((x:Int, y:Int) => x+y)
  }

  def max(t: Tree[Int]): Int = {
    fold[Int, Int](t)((x: Int) => x)((x: Int, y: Int) => Math.max(x,y))
  }

  def depth[A](t: Tree[A]): Int = {
    Math.floor(Math.log10(size(t))/Math.log10(2.0)).toInt + 1
  }

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    ???
  }

  def main (args: Array[String] ): Unit = {
      var t = new Branch[Int](new Leaf[Int](6), new Branch[Int](new Leaf[Int](4), new Leaf[Int](5)))
      println(fold[Int,Int](t)((x: Int) => x*x)((x:Int, y:Int) => x+y))
      println(size(t))
      println(max(t))
      println(depth(t))
      println(t)
  }

}