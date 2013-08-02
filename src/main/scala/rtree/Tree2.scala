package rtree

class Tree2 {
  
  var root: Node2 = createNode(new Array[WithShape](0))

  def insert(leaf: WithShape): Node2 = {
    root.insert(leaf) match {
      case Some(roots) => {
        val seq = roots.map(createNode(_))
        root = createNode(seq)
        root
      }
      case None => root
    }
  }

  def createNode(children: Seq[_ <: WithShape]): Node2 = {
    if (children.isEmpty || children.find(_.isInstanceOf[Node2]) == None) {
      new LeafNode(this, children)
    } else {
      new ContainerNode2(this, children)
    }
  }

  def split(nodes: Seq[_ <: WithShape]): Option[Seq[Seq[_ <: WithShape]]] = {
    None
  }

  def findTarget(shape: Shape, targets: Seq[_ <: WithShape]): Int = {
    0
  }
}

trait WithShape {
  def shape: Shape
}

class EmptyNodeException extends Exception

abstract class Node2(val tree: Tree2, var children: Seq[_ <: WithShape] = new Array[WithShape](0)) extends WithShape {
  def insert(l: WithShape): Option[Seq[Seq[_ <: WithShape]]]

  def shape: Shape = {
    if (children.isEmpty) {
      throw new EmptyNodeException
    }
    children.foldLeft[Option[Shape]](None)((acc, c) => Some(c.shape + acc.orNull)).orNull
  }
}

class LeafNode(tree: Tree2, readyChildren: Seq[_ <: WithShape] = new Array[WithShape](0)) extends Node2(tree, readyChildren) {

  def insert(l: WithShape): Option[Seq[Seq[_ <: WithShape]]] = {
    children = children :+ l
    tree.split(children)
  }
}

class ContainerNode2(tree: Tree2, readyChildren: Seq[_ <: WithShape] = new Array[WithShape](0)) extends Node2(tree, readyChildren) {

  override def insert(l: WithShape): Option[Seq[Seq[_ <: WithShape]]] = {
    val targetIndex = tree.findTarget(l.shape, children)

    val replacement = children(targetIndex) match {
      case n: Node2 => n.insert(l)
      case _ => None
    }
    replacement match {
      case Some(x) => {
        children = children.patch(targetIndex, x.map(tree.createNode(_)), 1);
        tree.split(children)
      }
      case None => None
    }    
  }
}