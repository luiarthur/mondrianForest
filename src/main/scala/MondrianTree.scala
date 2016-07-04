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

  /* Ideas: 
   *        Try writing a Tree Trait with dat, left, right, parent. (http://docs.scala-lang.org/tutorials/tour/traits.html)
   *        Then write a class that extends the trait to wrap called MT.
   */

  /*
  trait Node[T] {
    val dat: T
    val left: Node[T]
    val right: Node[T]
    var p: Node[T] = null
    Seq(right,left).foreach(x => if (x match {case null => false; case _ => true}) x.p= this)
  }

  class MT[T] (val dat: T, val left: MT[T], val right: MT[T]) extends Node[T] {
    lazy val parent = p // don't call parent before the tree has been extended!!! 
    def this(data: T) = this(data, null, null)
    override def toString(): String = "(" + dat + ", " + left + ", " + right + ")"
    def isLeaf(): Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot(): Boolean = parent match {case null => true; case _ => false}
  }
     val x = new MT(1)
     val y = new MT(2)
     x.parent // This calls lazy val initialization
     val z = new MT(3,x,y)
     // Interesting?
     z.left.parent
     z.right.parent
     val z = new MT(3,new MT(1),new MT(2))
   */
}
