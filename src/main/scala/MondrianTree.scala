package mondrian

object Mondrian {
  import scala.util.Random
  import Timer.time
  import breeze.linalg._
  private val rand = new Random(System.currentTimeMillis());
  
  /** @constructor Tree */
  case class Tree[T](val data: T, val left: Tree[T] = null, val right: Tree[T] = null) {
    var mutableParent: Tree[T] = null // Use 'parent' instead!!! 
    Seq(right,left).foreach(x => if (x match {case null => false; case _ => true}) x.mutableParent = this)
    def parent = mutableParent

    def isLeaf(): Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot(): Boolean = parent match {case null => true; case _ => false}
  }
}
