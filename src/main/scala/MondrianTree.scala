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
  class Node[T](val parent: Node[T] = null, val x: T, val l: Node[T] = null, val r: Node[T] = null) {
    override def toString(): String = "(" + "Parent: " + parent + ", " + 
                                            Console.GREEN + "Value: " + x + ", " + Console.RESET +
                                            "Left: " + l + ", " + 
                                            "Right: " + r + ")"
  }
}

/* Quick Tests:
  val node = new Node[Double](x=1, l=new Node(x=1), r=new Node(x=2))

  val mt = new MT(List(1,2,3), List(2,3,4), List(3,4,5))
  val mnode = new Node[MT](x=mt, l=new Node[MT](x=mt), r=new Node[MT](x=mt))
*/
