package map.base

import org.scalatest.FlatSpec

class ShapeTreeTest extends FlatSpec {

  class ShapeItem(val id: Long, override val payload: Shape) extends WithPayload[Shape] {
    override def toString = id + " : " + payload
    def computePayload() = payload
  }
  
  object ShapeItem {
    var id = 0
    
    def apply(shape : Shape) = {
      id += 1
      new ShapeItem(id - 1, shape)
    }
  }

  class BinaryShapeTreeStrategy extends ShapeTreeStrategy {
    
    def split(nodes: Seq[_ <: WithPayload[Shape]]): Option[Seq[Seq[_ <: WithPayload[Shape]]]] = {
      if (nodes.length > 2) {
        val a2: Seq[WithPayload[Shape]] = Array(nodes(0), nodes(1))
        val a1: Seq[WithPayload[Shape]] = Array(nodes(2))
        new Some(Array(a1, a2))
      } else {
        None
      }
    }
    
    def findTarget(payload: Shape, targets: Seq[_ <: WithPayload[Shape]]): Int = {
      targets.find(_.payload.contains(payload)) match {
        case None => 0
        case Some(x) => targets.indexOf(x)
      }
    }
  }

  "ShapeTree" should "work" in {
    val tree = new ShapeTree(new BinaryShapeTreeStrategy)

    tree.insert(ShapeItem(Shape(0, 0)));
    tree.insert(ShapeItem(Shape(1, 1)));
    tree.insert(ShapeItem(Shape(-1, -1)));
    tree.insert(ShapeItem(Shape(0.5, 0.5)));
    tree.insert(ShapeItem(Shape(2, -7)));
    tree.insert(ShapeItem(Shape(2, -7)));

    println(tree)
    assert(tree.payload == Shape(-1, 1, 2, -7))
  }

}