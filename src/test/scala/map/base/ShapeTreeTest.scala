package map.base

import org.scalatest.FlatSpec

class ShapeTreeTest extends FlatSpec {

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

  class BinaryShapeTreeStrategy extends ShapeTreeStrategy {

    def split(nodes: Seq[_ <: WithPayload[Shape]]) = {
      nodes.length match {
        case 3 => {
          val right: Seq[WithPayload[Shape]] = Array(nodes(0), nodes(1))
          val left: Seq[WithPayload[Shape]] = Array(nodes(2))
          Some(Array(left, right))
        }
        case _ => None
      }
    }

    def findTarget(payload: Shape, targets: Seq[_ <: WithPayload[Shape]]) = {
      targets.find(_.payload.contains(payload)) match {
        case Some(x) => targets.indexOf(x)
        case None => 0
      }
    }
  }

  "ShapeTree" should "work" in {
    val tree = new ShapeTree(new ShapeRTreeStrategy(4))

    tree.insert(ShapeItem(Shape(0, 0)));
    tree.insert(ShapeItem(Shape(1, 1)));
    tree.insert(ShapeItem(Shape(-1, -1)));
    tree.insert(ShapeItem(Shape(0.5, 0.5)));
    tree.insert(ShapeItem(Shape(2, -7)));
    tree.insert(ShapeItem(Shape(2, -7)));

    println(tree)
    assert(tree.payload == Shape(-1, 1, 2, -7))
  }

  "ShapeTree" should "work with more number of nodes" in {
    val tree = new ShapeTree(new ShapeRTreeStrategy(10))

    for (x <- 0 to 100) {
      tree.insert(ShapeItem(Shape(x, x)));
    }
    println(tree)
    assert(tree.payload == Shape(0, 0, 100, 100))
  }

}