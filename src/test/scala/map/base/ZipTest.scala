package map.base

import org.scalatest.FlatSpec
import scala.io.Source

class ZipTest extends FlatSpec {
  class ShapeItem(val id: Long, override val payload: Shape) extends WithPayload[Shape] {
    override def toString = id + " : " + payload
    def computePayload() = payload
  }

  object ShapeItem {
    var id = 0

    def apply(shape: Shape) = {
      id += 1
      new ShapeItem(id - 1, shape)
    }
  }

  "Tree" should "load zip data" in {
    val tree = new ShapeTree(new ShapeRTreeStrategy(10))

    val source = Source.fromURL(getClass.getResource("/zip.txt"))
    for (line <- source.getLines()) {
      val values = line.split('\t')
      val lat = values(9).toDouble
      val lon = values(10).toDouble
      tree.insert(ShapeItem(Shape(lat, lon)))
    }
    println("depth: " + tree.depth)
    println("area : " + tree.payload)
    
  }

}