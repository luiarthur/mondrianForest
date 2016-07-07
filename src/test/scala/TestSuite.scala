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

  test("Test mutablility is stable") {
     val v1 = Tree(1)
     val v2 = Tree(2)
     val v3 = Tree(3,Tree(4),Tree(5))

     v1.left = v2
     v1.right = v3
     val test1 = v1.left.parent == v1
     
     val v4 = Tree(6,Tree(7),v3)
     v1.right = v4
     assert(v1.right==v4 && v1.right != v3 && test1)
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
  val mt = new MT(D,10)

  test("Mondrian Tree SampleMT()") {
    val m = mt.sampleMT()
    //print(Console.BLUE+m.head.treeString+Console.RESET)
    //print(Console.GREEN+mt.sampleMT().head.treeString+Console.RESET)
    //assert(mt.sampleMT() != mt.sampleMT())
  }

  test("Mondrian Forest") {
    val mm = mt.sampleMT(n=100)
    //mm.foreach( _.draw )
  }

  test("Mondrian Tree ExtendMT()") {
    val mt = new MT(D,.3)
    val m = mt.sampleMT(n=100)
    val newDat = Data(Vector(1), Vector(Vector(10,2,3,4)) )
    print(Console.BLUE+m.head.treeString+Console.RESET)
    val mx = mt.extendMT(m.head,newDat)
    print(Console.GREEN+mx.head.treeString+Console.RESET)
    val newDat2 = Data(Vector(0), Vector(Vector(1,20,3,4)) )
    val mx2 = mt.extendMT(mx.head,newDat2,2)
    mx2.foreach(x => print(Console.YELLOW+x.treeString+Console.RESET) )
  }
}

