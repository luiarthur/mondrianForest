/**
  * Created by arthur on 7/4/16.
  */
object Mondrian {

  case class Data[T](y: Vector[T], X: Vector[Vector[Double]])

  case class MT[T](elem: T, left: MT[T] = null, right: MT[T] = null) {
    var mutableParent: MT[T] = null // Use only 'parent' in interface!!!
    Seq(right,left).foreach(child => if (child match {case null => false; case _ => true}) child.mutableParent = this)
    def parent = mutableParent
    def isLeaf: Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot: Boolean = parent match {case null => true; case _ => false}

    def sampleMT(lam: Double, X: Vector[Vector[Double]]): MT[T] = {
      def sampleMB(lam: Double, X: Vector[Vector[Double]]): MT[T] = ???

      ???
    }
  }
}
