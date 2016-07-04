package mondrian
import org.scalatest.FunSuite

class FunMondrianSuite extends FunSuite  {

  import mondrian.Mondrian.MT
  import mondrian.Timer.time

  val bLeft = MT(1)
  val bRight = MT(2)
  val bRoot = MT(0,bLeft,bRight)

  test("Test parent of children is self") {
    assert(bRoot.left.parent == bRoot && bRoot.right.parent == bRoot)
  }

  test("Test bRoot is Root") {
    assert(bRoot.isRoot && !bRoot.right.isRoot && !bRoot.left.isRoot)
  }

  test("Test bRoot.left and .right are Leafs") {
    assert(!bRoot.isLeaf && bRoot.left.isLeaf && bRoot.right.isLeaf)
  }

  test(Console.BOLD + "Copy Logic!!! THIS IS IMPORTANT TO UNDERSTAND!!!") {
    assert(bRight == bRoot.right && bLeft == bRoot.left)
  }

}
