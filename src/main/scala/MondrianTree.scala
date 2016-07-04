package mondrian

object Mondrian {
  import scala.util.Random
  import Timer.time
  import breeze.linalg._
  private val rand = new Random(System.currentTimeMillis());
  

  /** @constructor Tree */
  // Scala Book p. 701. Using case class because I can get a copy method for free.
  /*
  case class Tree[T](val data: T, val left: Tree[T] = null, val right: Tree[T] = null) {
    var parent: Tree[T] = null
    override def toString(): String = "(" + data + ", " + left + ", " + right + ")"
    Seq(right,left).foreach(x => if (x match {case null => false; case _ => true}) x.parent = this)
    def p = parent

    def isLeaf(): Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot(): Boolean = parent match {case null => true; case _ => false}
  }
  */

  /* Ideas: 
   *        Try writing a Tree Trait with dat, left, right, parent. (http://docs.scala-lang.org/tutorials/tour/traits.html)
   *        Then write a class that extends the trait to wrap called MT.
   */

  trait Node[T] {
    val dat: T
    val left: Node[T]
    val right: Node[T]
    var parentMutable: Node[T] = null // Don't ever use this! Use parent instead!
    Seq(right,left).foreach(x => if (x match {case null => false; case _ => true}) x.parentMutable= this)
  }

  case class MT[T] (val dat: T, val left: MT[T] = null, val right: MT[T] = null) extends Node[T] {
    def parent = parentMutable
    def isLeaf(): Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot(): Boolean = parent match {case null => true; case _ => false}
  }

  /*
  val x = Tree(1)
  val y = Tree(2)
  x.parent
  val z = Tree(3,x,y)
  // Interesting?
  x.parent
  z.left.parent
  z.right.parent
  z.parent
  val z = new MT(3,new MT(1),new MT(2))
  */
}
