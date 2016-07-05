/**
  * Created by arthur on 7/4/16.
  */
object Mondrian {
  /** @constructor MT */
  import breeze.linalg.{DenseMatrix,DenseVector}
  case class MT[T](data: T, left: MT[T] = null, right: MT[T] = null) {
    var mutableParent: MT[T] = null // Use only 'parent' in interface!!!
    Seq(right,left).foreach(child => if (child match {case null => false; case _ => true}) child.mutableParent = this)
    def parent = mutableParent

    def isLeaf: Boolean = (left,right) match {case (null,null) => true; case _ => false}
    def isRoot: Boolean = parent match {case null => true; case _ => false}

    def sampleMT(lam: Double, X: DenseMatrix[Double]): MT[T] = {
      def sampleMB(j: MT[T], lam: Double, X: DenseMatrix[Double]): MT[T] = ???
      ???
    }
  }
}
