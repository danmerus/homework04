package fintech.homework04
import fintech.homework04.Tree._
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  var t = new Branch[Int](new Leaf[Int](6), new Branch[Int](new Leaf[Int](4), new Leaf[Int](5)))
  var t1 = new Leaf[Int](-2)
  it should "apply fold correctly" in {
    fold[Int, Int](t)((x: Int) => x * x)((x: Int, y: Int) => x + y) shouldEqual 77
  }
  it should "apply max correctly1" in {
    max(t) shouldEqual 6
  }
  it should "apply max correctly2" in {
    max(t1) shouldEqual -2 // как фиксить непонятно
  }
  it should "apply size correctly1" in {
    Tree.size(t) shouldEqual 3
  }
  it should "apply size correctly2" in {
    Tree.size(t1) shouldEqual 1
  }
  it should "apply depth correctly1" in {
    depth(t) shouldEqual 2
  }
  it should "apply depth correctly2" in {
    depth(t1) shouldEqual 0
  }
  it should "apply map correctly1" in {
    def mfunc:Int => Int =  x => x*x
    max(map(t)(mfunc)) should equal(36)
  }
  it should "apply map correctly2" in {
    def mfunc:Int => Int =  x => x*x
    max(map(t1)(mfunc)) should equal(4)
  }
}
