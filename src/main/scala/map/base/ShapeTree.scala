package map.base

import scala.collection.mutable.ArrayBuffer

class ShapeTree(strategy: TreeStrategy[Shape]) extends Tree[Shape](strategy)

trait ShapeTreeStrategy extends TreeStrategy[Shape] {
  def combinePayload(nodes: Seq[_ <: WithPayload[Shape]]) = {
    if (nodes.isEmpty) {
      throw new EmptyNodeException
    }
    nodes.foldLeft[Option[Shape]](None)((acc, c) => Some(c.payload + acc.orNull)).orNull
  }
}

class ShapeRTreeStrategy(val maxFill: Int = 3) extends ShapeTreeStrategy {

  def split(nodes: Seq[_ <: WithPayload[Shape]]) = {
    if (nodes.length > maxFill) {
      val pairs = for (x <- nodes; y <- nodes if !(x eq y)) yield (x, y)
      val worstPairIndex = pairs.map((x) => (x._1.payload + x._2.payload).area).zipWithIndex.maxBy(_._1)._2

      val left = new ArrayBuffer[WithPayload[Shape]] += pairs(worstPairIndex)._1
      val right = new ArrayBuffer[WithPayload[Shape]] += pairs(worstPairIndex)._2

      var remaining = nodes.filterNot(x => (x eq left(0)) || (x eq right(0)))
      while (!remaining.isEmpty) {
        val leftPayload = combinePayload(left)
        val rightPayload = combinePayload(right)

        val associations = remaining.map(n =>
          ((leftPayload + n.payload).area,
            (rightPayload + n.payload).area)).zipWithIndex

        val ((minLeft, someRight), index0) = associations.minBy(_._1._1)
        val ((someLeft, minRight), index1) = associations.minBy(_._1._2)
        
        if (minLeft < minRight) {
          left += remaining(index0)
          remaining = remaining.patch(index0, Nil, 1)
        } else {
          right += remaining(index1)
          remaining = remaining.patch(index1, Nil, 1)
        }
      }
      Some(Array(left, right))
    } else {
      None
    }
  }

  def findTarget(payload: Shape, targets: Seq[_ <: WithPayload[Shape]]) = {
    targets.map((n) => (n.payload + payload).area - n.payload.area).zipWithIndex.minBy(_._1)._2
  }

}