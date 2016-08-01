/**
  * Created by arthur on 7/4/16.
  */
object Mondrian { // for Classification: IKN. (Implementation on real responses with Gaussian belief propagation will come later.)

  import breeze.linalg.{DenseMatrix,DenseVector}
  import breeze.stats.distributions.{Exponential,Multinomial,Uniform}

  case class Data(y: Vector[Double], X: Vector[Vector[Double]])
  case class Tup( var splitDim: Int = -1, var splitLoc: Double = 0, var splitTime: Double = 0, 
                  var l: Vector[Double]=null, var u: Vector[Double]=null,
                  var inds: Vector[Int] = Vector()) {
    override def toString(): String = if (splitDim == -1) "*" else "X" + (splitDim+1) + " < " + (splitLoc * 100).round / 100.0
  }
 
  case class Tree[T](private var _elem: T, private var _left: Tree[T] = null, private var _right: Tree[T] = null) {
    def elem_=(that: T): Unit = _elem = that
    def left_=(that: Tree[T]): Unit = {
      this._left = that
      that._parent = this
    }
    def right_=(that: Tree[T]): Unit = {
      this._right = that
      that._parent = this
    }
    def elem = _elem
    def parent = _parent
    def left = _left
    def right = _right

    private var _parent: Tree[T] = null
    Seq(_right,_left).foreach(child => if (child match {case null => false; case _ => true}) child._parent = this)

    def isLeaf: Boolean = (_left,_right) match {case (null,null) => true; case _ => false}
    def isRoot: Boolean = _parent match {case null => true; case _ => false}
    def nodes(): List[Tree[T]] = if (isLeaf) List(this) else left.nodes ::: right.nodes ::: List(this)

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
      val ls = if (_left.isLeaf) Vector(_left.elem.toString) else _left.pretty(spacing)
      val rs = if (_right.isLeaf) Vector(_right.elem.toString) else _right.pretty(spacing)
      val posL = ls(0).indexOf(_left.elem.toString)
      val posR = rs(0).indexOf(_right.elem.toString)
      val top = rep(posL) + rep(spacing+ls(0).size-posL,"_") + ps + rep(spacing+posR,"_") + rep(rs(0).size-posR)
      val bottom = List(ls, Vector(rep(spacing*2 + ps.size)), rs).reduce(paste)
      Vector(top) ++ bottom
    }

    def treeString(): String = if (isLeaf) "Leaf(" + elem.toString + ")" else "\n" + pretty(spacing=1).mkString("\n") + "\n"
    def draw(): Unit = println(treeString)
  }

  class MT(val data: Data, val lam: Double) {
    def sampleMT(dat: Data = data, n: Int=1): List[Tree[Tup]] = { // Algorithm 1
      (1 to n).toList.map( i => {
        val t = Tree(Tup(inds=(0 until dat.y.size).toVector))
        sampleMB(t)
        t
      })
    }

    def sampleMB(j: Tree[Tup], dat: Data = data): Unit = { // Algorithm 2. Mutable procedure
      val k = dat.X(0).size
      val subX = j.elem.inds.map(i => dat.X(i))
      val l = (0 until k).map(i => subX.map(x => x(i)).min).toVector
      val u = (0 until k).map(i => subX.map(x => x(i)).max).toVector
      j.elem.l = l
      j.elem.u = u
      val diffs = (u,l).zipped.map(_-_)
      val diffSum = diffs.sum
      val e = Exponential( diffSum + .00000001).draw
      if (diffSum > 0 && j.elem.splitTime + e < lam ) { // IF diffSum == 0 THEN split time -> infinity
        j.elem.splitTime = if (j.isRoot) e else j.parent.elem.splitTime + e
        j.elem.splitDim = Multinomial(new DenseVector(diffs.toArray)).sample
        j.elem.splitLoc = Uniform(l(j.elem.splitDim), u(j.elem.splitDim)).sample
        val (lInd, rInd) = j.elem.inds.partition(i => dat.X(i)(j.elem.splitDim) < j.elem.splitLoc )
        j.left = Tree(Tup(inds = lInd.toVector))
        j.right = Tree(Tup(inds = rInd.toVector))
        sampleMB(j.left)
        sampleMB(j.right)
      } else j.elem.splitTime = lam
    }

    def extendMT(tree: Tree[Tup], newDat: Data, n: Int = 1): List[Tree[Tup]] = { // Algorithm 4
      val x = newDat.X(0)
      val k = x.size

      def extendMB(j: Tree[Tup]): Unit = { // procedure, mutable
        val el = (j.elem.l zip x).map(w => List(w._1 - w._2,0).max)
        val eu = (j.elem.u zip x).map(w => List(w._2 - w._1,0).max)
        val es = (el zip eu).map(w => w._1 + w._2)
        val eSum = es.sum
        val e = Exponential( eSum + .00000001).draw
        val parentSplitTime = if (j.isRoot) 0 else j.parent.elem.splitTime
        if (eSum > 0 && parentSplitTime + e < j.elem.splitTime) {
          val delta = Multinomial(new DenseVector(es.toArray)).sample
          val xi = {if (x(delta) > j.elem.u(delta)) Uniform(j.elem.u(delta), x(delta)) else Uniform(x(delta), j.elem.l(delta))}.sample
          val jparent = if (j.isRoot) Tree(Tup()) else j.parent
          val jnew = Tree(Tup(splitDim=delta, splitLoc=xi, splitTime=jparent.elem.splitTime + e, 
                              l=(j.elem.l zip x).map(w => List(w._1,w._2).min), 
                              u=(j.elem.u zip x).map(w => List(w._1,w._2).max), inds=Vector(0)))
          val isLeftChild = !j.isRoot && j.parent.left == j
          if (isLeftChild) jparent.left=jnew else jparent.right=jnew

          if (x(delta) < xi) {
            jnew.left = Tree(Tup(inds=(0 until newDat.y.size).toVector))
            jnew.right = j
            sampleMB(jnew.left,newDat)
          } else {
            jnew.left = j
            jnew.right = Tree(Tup(inds=(0 until newDat.y.size).toVector))
            sampleMB(jnew.right,newDat)
          }
        } else {
          j.elem.l = (j.elem.l zip x).map(w => List(w._1,w._2).min) // mutable
          j.elem.u = (j.elem.u zip x).map(w => List(w._1,w._2).max) // mutable
          if (!j.isLeaf) {
            val child = if (x(j.elem.splitDim) < j.elem.splitLoc) j.left else j.right
            extendMB(child)
          }
        }
      }

      (1 to n).toList.map( i => {
        val t = tree.copy()
        extendMB(t)
        t
      })
    } // End of Algorithm 4

  }

  /*
    import Mondrian._
    val iris = scala.io.Source.fromFile("src/test/resources/iris.csv").getLines.map(x=>x.split(",").toVector.map(_.toDouble)).toVector
    val n = iris.size
    val k = iris(0).size - 1
    val y = iris.map(_(k))
    val X = iris.map(x => x.take(k))
    val D = Data(y,X)
    val mt = new MT(D,.2)
    val m = mt.sampleMT()
    m.head.draw
    val newDat = Data(Vector(1), Vector(Vector(1,2,3,4)))
    val mx = mt.extendMT(m.head,newDat)
    mx.head.draw
    mx.head.nodes.foreach( _.draw )
   */

}
