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

  /*
   val t1 = new Tree
   val tLeft = t1.update(n=Vector(1,2),l=Map(0->1),r=Map(0->2))
   t1.leaves
   */
}
