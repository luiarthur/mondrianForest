package mondrian
import org.scalatest.FunSuite

class FunTicTacSuite extends FunSuite  {

  import mondrian.Mondrian._
  import mondrian.Timer.time

  val root = new Tree
  val t1 = root.update(l=Map(0->1), r=Map(0->2))
  println(t1)

  test("Test") {
    assert(true)
  }

  test("Test isLeaf()") {
    assert(!t1.isLeaf(0) && t1.isLeaf(1) && t1.isLeaf(2) && !t1.isLeaf(3))
  }

  test("Test leaves()") {
    assert(t1.leaves.size==2 && root.leaves.size==1)
  }

  test("Test update with Tree") {
    // root.update(t1,t1)
    // Must prevent adding existing nodes!
    assert(false)
  }
}
