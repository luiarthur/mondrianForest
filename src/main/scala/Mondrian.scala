/**
  * Created by arthur on 7/4/16.
  */
object Mondrian {

  import breeze.linalg.{DenseMatrix,DenseVector}
  import breeze.stats.distributions.{Exponential,Multinomial,Uniform}

  case class Data(y: Vector[Double], X: Vector[Vector[Double]])
  case class Tup(splitDim: Int = -1, splitLoc: Double = 0, splitTime: Double = 0)

  case class Tree[T](elem: T, left: Tree[T] = null, right: Tree[T] = null) {
    var mutableParent: Tree[T] = null // Use only 'parent' in interface!!!
    Seq(right,left).foreach(child => if (child match {case null => false; case _ => true}) child.mutableParent = this)
    def parent = mutableParent
    def isLeaf: Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot: Boolean = parent match {case null => true; case _ => false}
    def pretty(): String = {
      def blanks(n: Int): String = List.fill(n)(" ").mkString
      def draw(tree: Tree[T], lw: Int=0, rw: Int=0, lh: Int=0, rh: Int=0): String = {
        ???
      }
      draw(this)
    }
  }
  
  class MT(val dat: Data, val lam: Double) {
    def sampleMT(): Tree[Tup] = { // Algorithm 1
      val k = dat.X(0).size
      val n = dat.y.size

      def sampleMB(subX: Vector[Vector[Double]], tup: Tup): Tree[Tup] = { // Algorithm 2
        val l = (0 until k).map(i => subX.map(x => x(i)).min)
        val u = (0 until k).map(i => subX.map(x => x(i)).max)
        val diffs = (u,l).zipped.map(_-_)
        val diffSum = diffs.sum
        val e = Exponential( diffSum + .00000001).draw
        if (diffSum != 0 && tup.splitTime + e < lam ) { // diffSum == 0 => split time -> infinity
          val tau = tup.splitTime + e
          val Multinom = new Multinomial(new DenseVector(diffs.toArray))
          val delta = Multinom.sample// sample split dim
          val Unif = new Uniform(l(delta), u(delta)) 
          val xi = Unif.sample // sample split loc
          val (leftX,rightX) = subX.partition(x => x(delta) < xi)
          Tree(Tup(delta, xi, tau), sampleMB(leftX.toVector, Tup(splitTime=tau)), sampleMB(rightX.toVector, Tup(splitTime=tau)))
        } else Tree( Tup(tup.splitDim, tup.splitLoc, lam) )
      }
      sampleMB( dat.X, Tup() )
    }
  }

  /*
    val iris = scala.io.Source.fromFile("src/test/resources/iris.csv").getLines.map(x=>x.split(",").toVector.map(_.toDouble)).toVector
    val n = iris.size
    val k = iris(0).size - 1
    val y = iris.map(_(k))
    val X = iris.map(x => x.take(k))
    val D = Data(y,X)
    val mt = new MT(D,1000000)
    val m = mt.sampleMT
   */

}
