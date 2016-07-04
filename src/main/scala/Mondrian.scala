/**
  * Created by arthur on 7/4/16.
  */
object Mondrian {
  /** @constructor Tree */
  case class Tree[T](data: T, left: Tree[T] = null, right: Tree[T] = null) {
    var mutableParent: Tree[T] = null // Use only 'parent' in interface!!!
    Seq(right,left).foreach(x => if (x match {case null => false; case _ => true}) x.mutableParent = this)
    def parent = mutableParent

    def isLeaf: Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot: Boolean = parent match {case null => true; case _ => false}
  }
}