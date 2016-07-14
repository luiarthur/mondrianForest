object Crazy {
  type VD = Vector[Double]
  type VDD = Vector[Vector[Double]]
  case class Data(y: Vector[Double], X: Vector[Vector[Double]])
  case class Tup( var splitDim: Int = -1, var splitLoc: Double = 0, var splitTime: Double = 0, 
                  var l: Vector[Double]=null, var u: Vector[Double]=null,
                  var inds: Vector[Int] = Vector()) {
    override def toString(): String = if (splitDim == -1) "*" else "X" + (splitDim+1) + " < " + (splitLoc * 100).round / 100.0
  }

}
