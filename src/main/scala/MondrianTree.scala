package mondrian

object Mondrian {
  import scala.util.Random
  import Timer.time
  import breeze.linalg._
  private val rand = new Random(System.currentTimeMillis());
  

  /** @constructor Tree */
  // Scala Book p. 701. Using case class because I can get a copy method for free.
  case class Tree[T](val data: T, val left: Tree[T], val right: Tree[T], var parent: Tree[T] = null) {
    def this(data: T) = this(data, null, null)
    override def toString(): String = "(" + data + ", " + left + ", " + right + ")"
    Seq(right,left).foreach(x => if (x match {case null => false; case _ => true}) x.parent = this)

    def isLeaf(): Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot(): Boolean = parent match {case null => true; case _ => false}
  }

}
