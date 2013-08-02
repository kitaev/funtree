package rtree

class Tree2 {
  var root: Node2 = new LeafNode(this)
  val maxFill = 2;

  def insert(leaf: Leaf2): Node2 = {
    root.insert(leaf) match {
      case Some(roots) => {
        val seq = Array(createNode(roots._1), createNode(roots._2))
        root = createNode(seq)
        root
      }
      case None => root
    }
  }

  def createNode(children: IndexedSeq[_ <: WithShape]): Node2 = {
    if (children.isEmpty || children.forall(_.isInstanceOf[Leaf])) {
      new LeafNode(this, children)
    } else {
      new ContainerNode2(this, children)
    }
  }

  def split(nodes: IndexedSeq[_ <: WithShape]): Option[(IndexedSeq[_ <: WithShape], IndexedSeq[_ <: WithShape])] = {
    None
  }

  def findTarget(shape: Shape, targets: IndexedSeq[_ <: WithShape]): Int = {
    0
  }
}

trait WithShape {
  def shape: Shape
}

class Leaf2(val shape: Shape) extends WithShape

abstract class Node2(val tree: Tree2, var children: IndexedSeq[_ <: WithShape] = new Array[WithShape](0)) extends WithShape {
  def insert(l: WithShape): Option[(IndexedSeq[_ <: WithShape], IndexedSeq[_ <: WithShape])]

  def shape: Shape = {
    children.foldLeft[Option[Shape]](None)((acc, c) => Some(c.shape + acc.orNull)).orNull
  }
}

class LeafNode(tree: Tree2, readyChildren: IndexedSeq[_ <: WithShape] = new Array[WithShape](0)) extends Node2(tree, readyChildren) {

  def insert(l: WithShape): Option[(IndexedSeq[_ <: WithShape], IndexedSeq[_ <: WithShape])] = {
    children = children :+ l
    tree.split(children)
  }
}

class ContainerNode2(tree: Tree2, readyChildren: IndexedSeq[_ <: WithShape] = new Array[WithShape](0)) extends Node2(tree, readyChildren) {

  override def insert(l: WithShape): Option[(IndexedSeq[_ <: WithShape], IndexedSeq[_ <: WithShape])] = {
    val targetIndex = tree.findTarget(l.shape, children)

    val replacement = children(targetIndex) match {
      case n: Node2 => n.insert(l)
    }

    replacement match {
      case Some(x) => {
        val seq = Array(tree.createNode(x._1), tree.createNode(x._2))
        children = children.patch(targetIndex, seq, 1);
      }
      case None =>
    }
    tree.split(children)
  }
}