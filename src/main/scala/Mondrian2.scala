object Mondrian2 {

  case class Tree[T](private var _elem: T, private var _left: Tree[T] = null, private var _right: Tree[T] = null) {
    def elem_=(that: T): Unit = _elem = that
    def parent_=(that: Tree[T]): Unit = {
      this._parent = that
      that._left = this
      that._right = this
    }
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

    def treeString(): String = if (isLeaf) toString else "\n" + pretty(spacing=3).mkString("\n") + "\n"
    def draw(): Unit = print(treeString)
  }

  /*
     val v1 = Tree(1)
     val v2 = Tree(2)
     val v3 = Tree(3,Tree(4),Tree(5))
     v1.left = v2
     v1.right = v3
     v1.draw
     v1.left.draw
     v1.right.draw
     v1.left.parent == v1
     
     val v4 = Tree(6,Tree(7),v3)
     v1.right = v4
     v1.right.draw
     v4.left.draw
     v4.right.draw
     v3.parent.draw
     v1.draw
   */
 
}
