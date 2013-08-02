package rtree

class Tree2[P](val strategy: TreeStrategy2[P]) extends TreeStrategy2[P] {

  var root: Node2[P] = createNode(new Array[WithPayload[P]](0))

  def insert(leaf: WithPayload[P]): Node2[P] = {
    root.insert(leaf) match {
      case Some(roots) => {
        val seq = roots.map(createNode(_))
        root = createNode(seq)
      }
      case None => 
    }
    root
  }

  def createNode(children: Seq[_ <: WithPayload[P]]): Node2[P] = {
    if (children.isEmpty || children.find(_.isInstanceOf[Node2[P]]) == None) {
      new LeafNode[P](this, children)
    } else {
      new ContainerNode2[P](this, children)
    }
  }

  def split(nodes: Seq[_ <: WithPayload[P]]): Option[Seq[Seq[_ <: WithPayload[P]]]] = {
    strategy.split(nodes)
  }

  def findTarget(payload: P, targets: Seq[_ <: WithPayload[P]]): Int = {
    strategy.findTarget(payload, targets)
  }
  
  def combinePayload(nodes: Seq[_ <: WithPayload[P]]) : P = {
    strategy.combinePayload(nodes)
    //nodes.foldLeft[Option[Payload]](None)((acc, c) => Some(c.payload + acc.orNull)).orNull
  }
}

trait TreeStrategy2[P] {
  def split(nodes: Seq[_ <: WithPayload[P]]): Option[Seq[Seq[_ <: WithPayload[P]]]]
  def findTarget(payload: P, targets: Seq[_ <: WithPayload[P]]): Int
  def combinePayload(nodes: Seq[_ <: WithPayload[P]]) : P
  
}

trait WithPayload[P] {
  def payload: P
}

abstract class Node2[P](val tree: Tree2[P], var children: Seq[_ <: WithPayload[P]] = new Array[WithPayload[P]](0)) extends WithPayload[P] {
  def insert(l: WithPayload[P]): Option[Seq[Seq[_ <: WithPayload[P]]]]

  def payload: P = {
    tree.combinePayload(children);    
  }
}

class LeafNode[P](tree: Tree2[P], readyChildren: Seq[_ <: WithPayload[P]] = new Array[WithPayload[P]](0)) extends Node2(tree, readyChildren) {

  def insert(l: WithPayload[P]): Option[Seq[Seq[_ <: WithPayload[P]]]] = {
    children = children :+ l
    tree.split(children)
  }
}

class ContainerNode2[P](tree: Tree2[P], readyChildren: Seq[_ <: WithPayload[P]] = new Array[WithPayload[P]](0)) extends Node2(tree, readyChildren) {

  override def insert(l: WithPayload[P]): Option[Seq[Seq[_ <: WithPayload[P]]]] = {
    val targetIndex = tree.findTarget(l.payload, children)

    val replacement = children(targetIndex) match {
      case n: Node2[P] => n.insert(l)
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