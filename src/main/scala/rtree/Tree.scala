package rtree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

class Tree(val strategy: TreeStrategy) {
  val root = container(new Array[Node](0))

  def this() = {
    this(DumbStrategy)
  }

  def container(children: Array[Node]): Container = {
    new Container(this, strategy, children)
  }

  def container(node: Node): Container = {
    container(Array(node))
  }

  def container(): Container = {
    container(new Array[Node](0))
  }

  def leaf(id: Long, shape: Shape): Leaf = {
    new Leaf(id, shape)
  }

  override def toString = {
    val acc = new StringBuilder
    root.foreach(acc ++= "  " * _ + _ + "\n")
    acc.toString
  }
}

abstract class TreeStrategy {
  def insertion(nodes: ArrayBuffer[Node], shape: Shape): (Tree, Long) => Unit
  def overflow(nodes: ArrayBuffer[Node]): (Tree) => Unit
}

object DumbStrategy extends TreeStrategy {

  def insertion(nodes: ArrayBuffer[Node], shape: Shape) = {
    // options:

    if (nodes.isEmpty || nodes.forall(_.isInstanceOf[Leaf])) {
      // 1) add as a new leaf node
      addLeaf(nodes, _: Tree, _: Long, shape)
    } else {
      val target = nodes.find(_.shape.contains(shape))
      // 2) add into one of the nodes
      // 3) or add container and then 2)
      (tree: Tree, id: Long) => {
        target match {
          case Some(t) => { t.asInstanceOf[Container].insert(id, shape); }
          case None => { nodes += tree.container(tree.leaf(id, shape)) }
        }
      }
    }
  }

  def overflow(nodes: ArrayBuffer[Node]) = {
    if (nodes.length > 2 && nodes.length % 2 == 0) {
      (tree: Tree) =>
        {
          val n1 = new ArrayBuffer[Node]
          val n2 = new ArrayBuffer[Node]
          nodes.sliding(2, 2).foreach(a => {
            n1 += a(0)
            n2 += a(1)
          })
          nodes.clear()
          nodes += tree.container(n1.toArray)
          nodes += tree.container(n2.toArray)
        }
    } else {
      (tree: Tree) => {}
    }
  }

  private def addLeaf(nodes: ArrayBuffer[Node], tree: Tree, id: Long, shape: Shape) = {
    nodes += tree.leaf(id, shape)
  }

}