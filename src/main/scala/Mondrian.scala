/**
  * Created by arthur on 7/4/16.
  */
object Mondrian {

  import breeze.linalg.{DenseMatrix,DenseVector}
  import breeze.stats.distributions.{Exponential,Multinomial,Uniform}

  case class Data(y: Vector[Double], X: Vector[Vector[Double]])
  case class Tup(splitDim: Int = -1, splitLoc: Double = 0, splitTime: Double = 0) {
    override def toString(): String = "X" + splitDim + " < " + (splitLoc * 100).round / 100.0
  }

  case class Tree[T](elem: T, left: Tree[T] = null, right: Tree[T] = null) {
    var mutableParent: Tree[T] = null // Use only 'parent' in interface!!!
    Seq(right,left).foreach(child => if (child match {case null => false; case _ => true}) child.mutableParent = this)
    def parent = mutableParent
    def isLeaf: Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot: Boolean = parent match {case null => true; case _ => false}

    //elem match {
    //  case t: Tup => println("I am a Tup!" + t.splitDim)
    //  case _ => println("! am not a Tup!")
    //}

    private def pretty(spacing: Int = 3): Vector[String] = {
      def rep(n: Int, s: String=" "): String = List.fill(n)(s).mkString

      def paste(l: Vector[String], r: Vector[String]): Vector[String] = {
        def elongate(vs: Vector[String]): Vector[String] = {
          val maxCol = vs.map(_.size).max
          vs.map( s => s + rep(maxCol - s.size) )
        }
        val maxRow = List(l,r).map(_.size).max
        val newlr = Vector(l,r).map(x => x ++ Vector.fill(maxRow-x.size)("")).map(elongate(_))
        val out = for (i <- (0 until maxRow)) yield newlr(0)(i) + newlr(1)(i)
        out.toVector
      }

      val ps = elem.toString
      val ls = if (left.isLeaf) Vector(left.elem.toString) else left.pretty(spacing)
      val rs = if (right.isLeaf) Vector(right.elem.toString) else right.pretty(spacing)
      val posL = ls(0).indexOf(left.elem.toString)
      val posR = rs(0).indexOf(right.elem.toString)
      val top = rep(posL) + rep(spacing+ls(0).size-posL,"_") + ps + rep(spacing+posR,"_") + rep(rs(0).size-posR)
      val bottom = List(ls, Vector(rep(spacing*2 + ps.size)), rs).reduce(paste)
      Vector(top) ++ bottom
    }

    def treeString(): String = if (isLeaf) toString else "\n" + pretty(spacing=3).mkString("\n") + "\n"
    def draw(): Unit = print(treeString)
  }

  /*
    Tree(1).draw
     val v = Tree(1,Tree(2),Tree(3,Tree(4),Tree(5)))
     val v2 = Tree(6,Tree(7),v)
     val v3 = Tree(8,v2,Tree(9))
     v2.treeString
     v3.draw
   */
  
  class MT(val dat: Data, val lam: Double) {
    def sampleMT(): Tree[Tup] = { // Algorithm 1
      val k = dat.X(0).size
      val n = dat.y.size

      def sampleMB(inds: Vector[Int], tup: Tup): Tree[Tup] = { // Algorithm 2
        val subX = inds.map(i => dat.X(i))
        val l = (0 until k).map(i => subX.map(x => x(i)).min)
        val u = (0 until k).map(i => subX.map(x => x(i)).max)
        val diffs = (u,l).zipped.map(_-_)
        val diffSum = diffs.sum
        val e = Exponential( diffSum + .00000001).draw
        if (diffSum != 0 && tup.splitTime + e < lam ) { // IF diffSum == 0 THEN split time -> infinity
          val tau = tup.splitTime + e
          val Multinom = new Multinomial(new DenseVector(diffs.toArray))
          val delta = Multinom.sample// sample split dim
          val Unif = new Uniform(l(delta), u(delta)) 
          val xi = Unif.sample // sample split loc
          val (lInd, rInd) = inds.partition(i => dat.X(i)(delta) < xi )
          Tree(Tup(delta, xi, tau), sampleMB(lInd.toVector, Tup(splitTime=tau)), sampleMB(rInd.toVector, Tup(splitTime=tau)))
        } else Tree( Tup(tup.splitDim, tup.splitLoc, lam) )
      }
      sampleMB( (0 until n).toVector, Tup() )
    }

    def extendMT(newDat: Data): Tree[Tup] = { // Algorithm 4
      ???
    }
  }

  /*
    val iris = scala.io.Source.fromFile("src/test/resources/iris.csv").getLines.map(x=>x.split(",").toVector.map(_.toDouble)).toVector
    val n = iris.size
    val k = iris(0).size - 1
    val y = iris.map(_(k))
    val X = iris.map(x => x.take(k))
    val D = Data(y,X)
    val mt = new MT(D,.2)
    val m = mt.sampleMT
    m.draw
   */

}
