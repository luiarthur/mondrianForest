object Mondrian2 {

  case class Tree[T](elem: T, private var left: Tree[T] = null, private var right: Tree[T] = null) {
    def setParent(that: Tree[T]): Tree[T] = {
      this.parent = that
      that.setLeft(this)
      that.setRight(this)
      this
    }
    def setLeft(that: Tree[T]): Tree[T] = {
      this.left = that
      that.parent = this
      this
    }
    def setRight(that: Tree[T]): Tree[T] = {
      this.right = that
      that.parent = this
      this
    }
    def getLeft(): Tree[T] = left
    def getRight(): Tree[T] = right
    def getParent(): Tree[T] = parent

    private var parent: Tree[T] = null
    Seq(right,left).foreach(child => if (child match {case null => false; case _ => true}) child.parent = this)

    def isLeaf: Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot: Boolean = parent match {case null => true; case _ => false}

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
     val v1 = Tree(1)
     val v2 = Tree(2)
     val v3 = Tree(3,Tree(4),Tree(5))
     v1.setLeft(v2)
     v1.setRight(v3)
     v1.draw
     v1.getLeft.draw
     v1.getRight.draw
     v1.getLeft.getParent == v1
     
     val v4 = Tree(6,Tree(7),v3)
     v1.setRight(v4).draw
     v4.getLeft.draw
     v4.getRight.draw
     v3.getParent.draw
     v1.draw
   */
 
}
