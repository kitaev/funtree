package rtree

import org.scalatest.FlatSpec

class Tree2Test extends FlatSpec {

  class ShapeItem(val id: Long, val payload: Shape) extends WithPayload[Shape] {
    override def toString = id + " : " + payload
  }
  
  object ShapeItem {
    var id = 0
    
    def apply(shape : Shape) = {
      id += 1
      new ShapeItem(id - 1, shape)
    }
  }

  class ShapeStrategy extends TreeStrategy2[Shape] {
    
    def split(nodes: Seq[_ <: WithPayload[Shape]]): Option[Seq[Seq[_ <: WithPayload[Shape]]]] = {
      if (nodes.length > 2) {
        val a2: Seq[WithPayload[Shape]] = Array[WithPayload[Shape]](nodes(0), nodes(1))
        val a1: Seq[WithPayload[Shape]] = Array[WithPayload[Shape]](nodes(2))
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

    def combinePayload(nodes: Seq[_ <: WithPayload[Shape]]): Shape = {
      if (nodes.isEmpty) {
        throw new EmptyNodeException
      }
      nodes.foldLeft[Option[Shape]](None)((acc, c) => Some(c.payload + acc.orNull)).orNull
    }

  }

  "Tree2" should "work" in {
    val strategy = new ShapeStrategy
    val tree = new Tree2[Shape](strategy)

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