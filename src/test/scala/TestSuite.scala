package mondrian
import org.scalatest.FunSuite

class FunMondrianSuite extends FunSuite  {

  import mondrian.Mondrian._
  import mondrian.Timer.time

  val root = new Tree(Set(0))
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
    // Must prevent adding existing nodes!
    //val t2 = root.update(t1)
    //println(t2)
    assert(false)
  }
}
