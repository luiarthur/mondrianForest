package mondrian

object Mondrian {
  import scala.util.Random
  import Timer.time
  import breeze.linalg._
  private val rand = new Random(System.currentTimeMillis());
  

  /** @constructor Tree */
  class Tree(val n: Set[Int], val p: Map[Int,Int], val l: Map[Int,Int], val r: Map[Int,Int]) {

    def update(l: Map[Int,Int] = Map(), r: Map[Int,Int] = Map()): Tree = {
      val pOut = r.map(_.swap) ++ l.map(_.swap)
      val nOut = ( l.values ++ r.values ).toSet
      new Tree(nOut ++ this.n, pOut ++ this.p, l ++ this.l, r ++ this.r)
    }

    override def toString(): String = "(" + Console.GREEN + n + ", " + Console.RESET + p + ", " + l + ", " + r + ")"
    def this(n: Set[Int] = Set()) = this(n, Map(), Map(), Map())
    def leaves(): Set[Int] = (n.toSet diff (l ++ r).keys.toSet)
    def isLeaf(i: Int): Boolean = leaves() contains i
  }

  /** @constructor MT (Mondrian Tree Tuple)*/
  class MT(val T: Tree, val d: Map[Int,Int], val x: Map[Int,Double], val tau: Map[Int, Double]) {
    def this() = this(new Tree, Map(), Map(), Map()) // T: Tree, d: split dim, x: split loc, tau: split time
    def sampleMT(lam: Double, D: DenseMatrix[Double]): MT = { // D is a matrix, first col is y, the rest are X
      def sampleMB(j: Int, dj: DenseVector[Double], lam: Double): MT = {
        ???//val newT = T.update()
      }

      val mt = new MT
      //mt.sampleMB(0,D(0,::),lam)
      ???
    }
  }


  /* BTNode / BT
  class BTNode(val j: Int, val l: BTNode = null, val r: BTNode = null) {
    override def toString(): String = "(" + Console.GREEN + j + ", " + Console.RESET + l + ", " + r + ")"
    def this() = this(-1,null,null)
  }

  class BT(val j: Int, l: Option[BT] = None, r: Option[BT] = None, rt: Option[BTNode] = None) {
    def this() = this(-1, None, None, None)

    val left = l getOrElse null
    val right = r getOrElse null
    val root: BTNode = rt match {
      case Some(rt) => new BTNode(j, left.root, right.root)
      case _ => null
    }
  }
  */
  
  //class mutant(var a:Int, val b:Int) {override def toString(): String = "(" + a + "," + b + ")"}

  //val relations: Map[String,Map[BTNode,BTNode]] = Map("l" -> Map(), "r" -> Map(), "p" -> Map())
  //class BTNode(val j: Int, val l: BTNode, val r: BTNode, 
  //  val r0: Map[String,Map[BTNode,BTNode]] = Map("l" -> Map(), "r" -> Map(), "p" -> Map())) {

  //  def this() = this(-1, null, null)
  //  def this(j: Int) = this(j, null, null)

  //  val rel = Map("l" -> (Map(this -> l) ++ r0("l")), 
  //                "r" -> (Map(this -> r) ++ r0("r")), 
  //                "p" -> (Map(r-> this, l-> this) ++ r0("p")))

  //  val relations = { for (k <- rel.keys) yield {( k, rel(k) ++ l.rel(k) ++ r.rel(k) )} }.toMap
  //  //val relations = (l,r) match {
  //  //  case (Some(l),Some(r)) => { for (k <- rel.keys) yield {( k, rel(k) ++ l.rel(k) ++ r.rel(k) )} }.toMap
  //  //  case _ => null
  //  //}

  //  val left = l match {
  //    case Some(l) => l
  //    case _ => None
  //  }

  //  val right = r match {
  //    case Some(r) => r
  //    case _ => None
  //  }

  //  val parent = rel("p")

  //  override def toString(): String = "(" + Console.GREEN + j + ", " + Console.RESET + l + ", " + r + ")"
  //}

  /*
     new BTNode(0)
     new BTNode(1)
     val n2 = new BTNode(2,Some(new BTNode(0)),Some(new BTNode(1)))
     n2.relations
     n2.relations("l")
     n2.relations("r")
     n2.relations("p")

     val z = new BT
     val z1 = new BT(1, Some(z), Some(z))
   */


  /*
   val t1 = new Tree
   val tLeft = t1.update(n=Vector(1,2),l=Map(0->1),r=Map(0->2))
   t1.leaves
   */

  // Scala Book p. 701
  case class Tree[T](val elem: T, val left: Tree[T], val right: Tree[T], var parent: Tree[T]) {
    override def toString(): String = "(" + elem + ", " + left + ", " + right + ")"
    Seq(right,left).foreach(x => if (x match {case null => false; case _ => true}) x.parent = this)
    def isLeaf(): Boolean = left match {case null => true; case _ => false} // strictly rooted binary tree has either 0 or 2 nodes. So, just check if one of the nodes is null
    def isRoot(): Boolean = parent match {case null => true; case _ => false}
  }
  /*
     val b1 = new Tree(1,null,null,null)
     val b2 = new Tree(2, b1.copy(), null, null)
     b2.left.parent == b2
     b2.isLeaf
     b2.left.isLeaf
     b1.isRoot
     b2.left.isRoot
     b1 == b2.left
   */

}

/*
 abstract class Expr
  case class Var(name: String) extends Expr
  case class Number(num: Double) extends Expr
  case class UnOp(operator: String, arg: Expr) extends Expr
  case class BinOp(operator: String, left: Expr, right: Expr) extends Expr
*/

/*
 //http://stackoverflow.com/questions/2895057/scala-a-class-declaring-itself-as-a-variable
case class Tree[T](value: T, left: Option[Tree[T]], right: Option[Tree[T]]) {
  var parent: Tree[T] = null
  def setParent(tree: Tree[T]) { Seq(left, right).flatten.foreach(_.setParent(this)) }
}

object Tree {
  def getPath[T](tree: Tree[T]):List[T] = List(tree.value) ++
    (if(tree.parent == null) 
      Nil
    else
      getPath(tree.parent))
}
*/

/*
   val tLeft = Tree(1,None,None)
   val tRight = Tree(2,None,None)
   val tRoot = Tree(0,Some(tLeft),Some(tRight))
 */
