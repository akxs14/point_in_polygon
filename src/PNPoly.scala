
class Point(newX: Double, newY: Double) {
  var x: Double = newX
  var y: Double = newY

  override def toString(): String = "(" + x + ", " + y + ")"
} 

object PNPoly {
 /*
  * isLeft(): Tests if a point is left, on or right of an
  *           infinite line.
  *
  * Input:
  *       p0: The point of origin of a polygon line.
  *       p1: The point of termination of a polygon line.
  *       p2: The point whose location is to be determined.
  *
  * Return:
  *       > 0 when p2 lies left of the line between p0 and p1.
  *       = 0 when p2 lies on the line.
  *       < 0 when p2 lies right of the line.
  */
  def isLeft(p0: Point, p1: Point, p2: Point): Double = {
    ((p1.x - p0.x) * (p2.y - p0.y) + (p2.x - p0.x) * (p1.y - p0.y))
  }


 /*
  * calcCrossingNumber(): Performs the crossing number test for a point
  *                       and a polygon.
  *           infinite line.
  *
  * Input:
  *       p: The point to examine in the test.
  *       polygon: The polygon used in the test.
  *
  * Return:
  *       0: Point p is outside of the polygon.
  *       1: Point p is inside of the polygon.
  */
  def calcCrossingNumber(p: Point, polygon: Vector[Point]): Int = {
    var wn: Int = 0
    var n: Int = polygon.length - 1

    for(i <- 0 to n) {
      if(polygon(i).y <= p.y) {
        if(polygon((i+1) % polygon.length).y > p.y)
          if(isLeft(polygon(i), polygon((i+1) % polygon.length), p) > 0)
            wn = wn + 1
            println("up")
      }
      else {
        if(polygon((i+1) % polygon.length).y <= p.y)
          if(isLeft(polygon(i), polygon((i+1) % polygon.length), p) < 0)
            wn = wn - 1
            println("down")
      }
    }
    wn
  }


  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]) {
    val pa0 = new Point(5,5)
    val pb0 = new Point(10,5)
    val pc0 = new Point(15,5)

    val p1 = new Point(10,10)
    val p2 = new Point(10,2)
    val p3 = new Point(10,2)
    val poly = Vector(p1,p2,p3)

    println("isLeft pa0: %f", isLeft(pa0, p1, p2))
    println("isLeft pb0: %f", isLeft(pb0, p1, p2))
    println("isLeft pc0: %f", isLeft(pc0, p1, p2))


    println("calcCrossingNumber pa0: %f", calcCrossingNumber(pa0,poly))
    println("calcCrossingNumber pb0: %f", calcCrossingNumber(pb0,poly))
    println("calcCrossingNumber pc0: %f", calcCrossingNumber(pc0,poly))
  }
}
