/**
  * Created by arthur on 7/4/16.
  */
object Mondrian { // for Classification: IKN. (Implementation on real responses with Gaussian belief propagation will come later.)

  import math.log
  val Rand = scala.util.Random

  def rmultinom(p: Array[Double]) = {
    val u = Rand.nextDouble
    val cum = p.scanLeft(0.0)(_+_).tail
    val ind = cum.zipWithIndex.dropWhile(u > _._1).head._2
    ind
  }
  def runif(l: Double, u: Double) = Rand.nextDouble * (u-l) + l
  def rexp(rate: Double) = -log(1-Rand.nextDouble) / rate

  def dataRange(X: Vector[Vector[Double]]) = 
    Vector.range(0,X(0).size) map {j => 
      val colj = X map {x => x(j)}
      (colj.max, colj.min)
    }

  case class Tree[T](val elem: T, private var _left: Tree[T] = null, private var _right: Tree[T] = null) {
    def left_=(that: Tree[T]): Unit = {
      this._left = that
      that._parent = this
    }
    def right_=(that: Tree[T]): Unit = {
      this._right = that
      that._parent = this
    }
    def parent = _parent
    def left = _left
    def right = _right

    private var _parent: Tree[T] = null
    Seq(_right,_left).foreach(child => if (child match {case null => false; case _ => true}) child._parent = this)

    def isLeaf = (_left,_right) match {case (null,null) => true; case _ => false}
    def isRoot = _parent match {case null => true; case _ => false}
    def nodes: List[Tree[T]] = if (isLeaf) List(this) else left.nodes ::: right.nodes ::: List(this)
    def root = if (isRoot) this else this.parent

    private def pretty(spacing: Int = 3): Vector[String] = {
      def rep(n: Int, s: String=" ") = List.fill(n)(s).mkString

      def paste(l: Vector[String], r: Vector[String]) = {
        def elongate(vs: Vector[String]) = {
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

    def treeString = if (isLeaf) "Leaf(" + elem.toString + ")" else "\n" + pretty(spacing=1).mkString("\n") + "\n"
    def draw = println(root.treeString)
  }

  // WORK
  case class MT(lam: Double, xrng: Vector[(Double,Double)], numClasses: Int) {
    private val xdim = xrng.size
    private val defaultL = Array.fill(xdim)(1E100)
    private val defaultU = Array.fill(xdim)(-1E100)
    private var _tree = Tree( Info() )
    def tree = _tree

    case class Info(var splitDim: Int = -1, var splitLoc: Double = 0, var splitTime: Double = 0, 
                    var l: Array[Double]=defaultL, var u: Array[Double]=defaultU) {
      override def toString = if (splitDim == -1) "*" else "X" + (splitDim+1) + " < " + (splitLoc * 100).round / 100.0
    }

    private var init = true
    def update(x: Vector[Double], y: Int) = 
      if (init) {
        sampleMT(x,y)
        init = false
      } else extendMT(x,y)

    private def sampleMT(x: Vector[Double], y: Int) = sampleMB(_tree, x, y) // Algorithm 1
    private def sampleMB(j: Tree[Info], x: Vector[Double], y: Int): Unit = { // Algorithm 2
      for (d <- 0 until xdim) {
        j.elem.l(d) = scala.math.min(x(d),j.elem.l(d))
        j.elem.u(d) = scala.math.max(x(d),j.elem.u(d))
      }
      val diffs = (j.elem.u, j.elem.l).zipped.map(_-_)
      val rate = diffs.sum
      val E = rexp(rate + 1E-10)
      if (rate > 0 && j.elem.splitTime + E < lam ) {
        //diffs.foreach(cc => print(cc+"  "))
        val dim = rmultinom(diffs)
        val loc =  runif(j.elem.l(dim), j.elem.u(dim))
        j.elem.splitDim = dim
        j.elem.splitLoc = loc
        j.elem.splitTime = if (j.isRoot) E else j.parent.elem.splitTime + E

        //val ls = List(x(dim),loc)
        //j.left  = Tree(Info(l=j.elem.l.updated(dim,ls.min), u=j.elem.u.updated(dim,ls.max)))
        //j.right = Tree(Info(l=j.elem.l.updated(dim,ls.min), u=j.elem.u.updated(dim,ls.max)))
        j.left  = Tree(Info(l=j.elem.l, u=j.elem.u))
        j.right = Tree(Info(l=j.elem.l, u=j.elem.u))

        _tree.draw
        sampleMB(j.left,x,y)
        sampleMB(j.right,x,y)

      } else j.elem.splitTime = lam
    }

    private def extendMT(x: Vector[Double], y: Int) = extendMB(_tree.root,x,y) // Algorithm 3
    private def extendMB(j: Tree[Info], x: Vector[Double], y: Int): Unit = { // Algorithm 4
      val el = (j.elem.l zip x).map(w => List(w._1 - w._2,0).max)
      val eu = (j.elem.u zip x).map(w => List(w._2 - w._1,0).max)
      val es = (el zip eu).map(w => w._1 + w._2)
      val eSum = es.sum
      val e = rexp( eSum + .00000001)
      val parentSplitTime = if (j.isRoot) 0 else j.parent.elem.splitTime
      if (eSum > 0 && parentSplitTime + e < j.elem.splitTime) {
        val delta = rmultinom(es.toArray)
        val xi = {if (x(delta) > j.elem.u(delta)) runif(j.elem.u(delta), x(delta)) else runif(x(delta), j.elem.l(delta))}
        val jparent = if (j.isRoot) Tree(Info()) else j.parent
        val jnew = Tree(Info(splitDim=delta, splitLoc=xi, splitTime=jparent.elem.splitTime + e, 
                            l=(j.elem.l zip x).map(w => List(w._1,w._2).min), 
                            u=(j.elem.u zip x).map(w => List(w._1,w._2).max)))
        val isLeftChild = !j.isRoot && j.parent.left == j
        if (isLeftChild) jparent.left=jnew else jparent.right=jnew

        val ls = List(x(delta), xi)
        if (x(delta) < xi) {
          jnew.left = Tree(Info(l=j.elem.l.updated(delta,ls.min), u=j.elem.u.updated(delta,ls.max)))
          jnew.right = j
          sampleMB(jnew.left,x,y)
        } else {
          jnew.left = j
          jnew.right = Tree(Info(l=j.elem.l.updated(delta,ls.min), u=j.elem.u.updated(delta,ls.max)))
          sampleMB(jnew.right,x,y)
        }
      } else {
        j.elem.l = (j.elem.l zip x).map(w => List(w._1,w._2).min) // mutable
        j.elem.u = (j.elem.u zip x).map(w => List(w._1,w._2).max) // mutable
        if (!j.isLeaf) {
          val child = if (x(j.elem.splitDim) < j.elem.splitLoc) j.left else j.right
          extendMB(child,x,y)
        }
      }
    } // End of Algorithm 4
  }
}
