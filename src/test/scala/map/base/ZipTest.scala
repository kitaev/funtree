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
  
  def loadZipTree(resource : String, maxFill : Int) : ShapeTree = {
    val tree = new ShapeTree(new ShapeRTreeStrategy(maxFill))

    val source = Source.fromURL(getClass.getResource(resource))
    for (line <- source.getLines()) {
      val values = line.split('\t')
      val lat = values(9).toDouble
      val lon = values(10).toDouble
      tree.insert(ShapeItem(Shape(lat, lon)))
    }
    tree
  }

  "Tree" should "load zip data" in {
    val tree = loadZipTree("/zip.txt", 10)
    println("depth: " + tree.depth)
    println("area : " + tree.payload)
    
  }

  "Query" should "provide result" in {
    val tree = loadZipTree("/zip.txt", 10)
    assert(tree.query(Shape(50,10,51,11)).length > 0)
    assert(tree.query(Shape(10,10,11,11)).length == 0)
  }

}