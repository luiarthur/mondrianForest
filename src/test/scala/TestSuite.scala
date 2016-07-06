import org.scalatest.FunSuite

class TestSuite extends FunSuite  {

  import Mondrian._

  val bLeft  = Tree(1)
  val bRight = Tree(2)
  val bRoot  = Tree(0,bLeft.copy(),bRight)
  val bigRoot = Tree(bRoot.elem,bLeft,bRight)

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

  test("Mondrian Trees Int") {
    class MTInt(override val elem: Int, override val left: MTInt = null, override val right: MTInt = null) extends 
      Tree[Int](elem, left, right)

    val mti1 = new MTInt(1)
    val mti2 = new MTInt(2)
    val mti3 = new MTInt(1,mti1,mti2)

    assert(mti3.left == mti1 && mti3 == mti3.right.parent && !mti3.isLeaf && mti3.isRoot && mti1.isLeaf 
      && !mti1.isRoot && !mti3.left.isRoot)
  }

  //val irisDat = breeze.linalg.csvread(new java.io.File("src/test/resources/iris.csv"),',')
  //val irisVec = irisDat.toArray.toVector.grouped(150).toVector
  val iris = scala.io.Source.fromFile("src/test/resources/iris.csv").getLines.map(x=>x.split(",").toVector.map(_.toDouble)).toVector
  val n = iris.size
  val k = iris(0).size - 1
  val y = iris.map(_(k))
  val X = iris.map(x => x.take(k))
  val D = Data(y,X)

  //bRight.sampleMT(1,X)
}
