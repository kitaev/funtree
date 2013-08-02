package rtree

class Tree2(val strategy: TreeStrategy2) extends TreeStrategy2 {

  var root: Node2 = createNode(new Array[WithPayload](0))

  def insert(leaf: WithPayload): Node2 = {
    root.insert(leaf) match {
      case Some(roots) => {
        val seq = roots.map(createNode(_))
        root = createNode(seq)
      }
      case None => 
    }
    root
  }

  def createNode(children: Seq[_ <: WithPayload]): Node2 = {
    if (children.isEmpty || children.find(_.isInstanceOf[Node2]) == None) {
      new LeafNode(this, children)
    } else {
      new ContainerNode2(this, children)
    }
  }

  def split(nodes: Seq[_ <: WithPayload]): Option[Seq[Seq[_ <: WithPayload]]] = {
    strategy.split(nodes)
  }

  def findTarget(payload: Payload, targets: Seq[_ <: WithPayload]): Int = {
    strategy.findTarget(payload, targets)
  }
  
  def combinePayload(nodes: Seq[_ <: WithPayload]) : Payload = {
    strategy.combinePayload(nodes)
    //nodes.foldLeft[Option[Payload]](None)((acc, c) => Some(c.payload + acc.orNull)).orNull
  }
}

trait TreeStrategy2 {
  def split(nodes: Seq[_ <: WithPayload]): Option[Seq[Seq[_ <: WithPayload]]]
  def findTarget(payload: Payload, targets: Seq[_ <: WithPayload]): Int
  def combinePayload(nodes: Seq[_ <: WithPayload]) : Payload
  
}

trait Payload

trait WithPayload {
  def payload: Payload
}

class EmptyNodeException extends Exception

abstract class Node2(val tree: Tree2, var children: Seq[_ <: WithPayload] = new Array[WithPayload](0)) extends WithPayload {
  def insert(l: WithPayload): Option[Seq[Seq[_ <: WithPayload]]]

  def payload: Payload = {
    if (children.isEmpty) {
      throw new EmptyNodeException
    }
    tree.combinePayload(children);    
  }
}

class LeafNode(tree: Tree2, readyChildren: Seq[_ <: WithPayload] = new Array[WithPayload](0)) extends Node2(tree, readyChildren) {

  def insert(l: WithPayload): Option[Seq[Seq[_ <: WithPayload]]] = {
    children = children :+ l
    tree.split(children)
  }
}

class ContainerNode2(tree: Tree2, readyChildren: Seq[_ <: WithPayload] = new Array[WithPayload](0)) extends Node2(tree, readyChildren) {

  override def insert(l: WithPayload): Option[Seq[Seq[_ <: WithPayload]]] = {
    val targetIndex = tree.findTarget(l.payload, children)

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