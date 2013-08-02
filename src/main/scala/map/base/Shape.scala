package map.base

object Shape {
  def apply(x : Double, y : Double) : Shape = {
    Point(x, y)
  }

  def apply(x : Double, y : Double, x1 : Double, y1 : Double) : Shape = {
    Rect(x, y, x1, y1)
  }
}

abstract class Shape {

  def contains(that: Shape): Boolean

  def +(that: Shape): Shape = {
    if (that == null || (this contains that)) {
      this
    } else if (that contains this) {
      that
    } else {
      this expand that
    }
  }

  def expand(that: Shape): Shape
  
  def area : Double
}

object Point {
  def apply(x: Double, y: Double): Point = {
    new Point(x, y)
  }
}

class Point(val x: Double = 0, val y: Double = 0) extends Shape {

  def contains(that: Shape) = {
    that == this
  }

  def expand(that: Shape) : Shape = {
    that match {
      case p: Point => Rect(this, p)
      case r: Rect => r.expand(this)
    }
  }
  
  def area = 0.0

  override def equals(that: Any) = {
    that match {
      case p: Point => p.x == x && p.y == y
      case _ => false
    }
  }
  override def hashCode() = x.toInt + 13 * y.toInt
  override def toString = x + " x " + y

}

object Rect {
  def apply(x: Double, y: Double, x1: Double, y1: Double): Rect = {
    Rect(Point(x, y), Point(x1, y1))
  }

  def apply(p: Point, p1: Point): Rect = {
    new Rect(p, p1)
  }
}

class Rect(p0 : Point, p1 : Point) extends Shape {
  
  val topleft : Point = Point(p0.x.min(p1.x), p0.y.max(p1.y))
  val bottomright : Point = Point(p0.x.max(p1.x), p0.y.min(p1.y))  

  def contains(that: Shape) = {
    that match {
      case r: Rect => contains(r.topleft) && contains(r.bottomright)
      case p: Point => p.x >= topleft.x && p.x <= bottomright.x && p.y <= topleft.y && p.y >= bottomright.y 
      case _ => false
    }
  }

  def expand(that: Shape) = {
    that match {
      case r: Rect => Rect(topleft.x.min(r.topleft.x), topleft.y.max(r.topleft.y), bottomright.x.max(r.bottomright.x), bottomright.y.min(r.bottomright.y))
      case p: Point => Rect(topleft.x.min(p.x), topleft.y.max(p.y), bottomright.x.max(p.x), bottomright.y.min(p.y))
    }
  }

  def area = {((bottomright.x - topleft.x) * (topleft.y - bottomright.y)).abs}

  override def equals(that: Any) = {
    that match {
      case r: Rect => r.topleft == topleft && r.bottomright == bottomright
      case _ => false
    }
  }
  override def hashCode() = topleft.hashCode + 13 * bottomright.hashCode
  override def toString = topleft + " : " + bottomright
}