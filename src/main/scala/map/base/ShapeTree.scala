package map.base

class ShapeTree(strategy: TreeStrategy[Shape]) extends Tree[Shape](strategy)

trait ShapeTreeStrategy extends TreeStrategy[Shape] {
  def combinePayload(nodes: Seq[_ <: WithPayload[Shape]]): Shape = {
    if (nodes.isEmpty) {
      throw new EmptyNodeException
    }
    nodes.foldLeft[Option[Shape]](None)((acc, c) => Some(c.payload + acc.orNull)).orNull
  }
}