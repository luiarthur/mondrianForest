package mondrian

object Mondrian {
  import scala.util.Random
  import Timer.time
  private val rand = new Random(System.currentTimeMillis());
  
  /** @constructor Mondrian Tree */
  class MT(val T: List[Int], val delta: List[Double], val xi: List[Double]) {
    override def toString(): String = ???
    /**
     * addLeft
     * addRight
     * getLeft
     * getRight
     */
  }

  /** @constructor Node */
  class Node[T](val x: T, val l: Node[T] = null, val r: Node[T] = null, val p: Node[T] = null) {
    override def toString(): String = "(" + Console.GREEN + x + ", " + Console.RESET +
                                            l + ", " + r + "," + p + ")"
  }

  class Tree(val T: List[Int], val p: Map[Int,Int], val l: Map[Int,Int], val r: Map[Int,Int]) {
    def addLeft(m: Map[Int,Int]): Tree = new Tree(T,p,l ++ m,r)
    def addRight(m: Map[Int,Int]): Tree = new Tree(T,p,l,r ++ m)
    def addParent(m: Map[Int,Int]): Tree = new Tree(T,p ++ m,l,r)
    def this(T: List[Int]) = this(T,Map[Int,Int](),Map[Int,Int](),Map[Int,Int]())
  }

}

/* Quick Tests:
  val node = new Node[Double](x=1, l=new Node(x=1), r=new Node(x=2), p=new Node(x=0))
  val node2 = new Node[Double](x=2, l=node, r=node)

  val mt = new MT(List(1,2,3), List(2,3,4), List(3,4,5))
  val mnode = new Node[MT](x=mt, l=new Node[MT](x=mt), r=new Node[MT](x=mt))
*/
