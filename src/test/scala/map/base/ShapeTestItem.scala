package map.base

class ShapeTestItem(val id: Long, override val payload: Shape) extends WithPayload[Shape] {
  override def toString = id + " : " + payload
  def computePayload() = payload
}

object ShapeTestItem {
  var id = 0

  def apply(shape: Shape) = {
    id += 1
    new ShapeTestItem(id - 1, shape)
  }
}
