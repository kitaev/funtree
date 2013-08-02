package rtree

import scala.collection.mutable.ArrayBuffer

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
  
  def payload : P = root.payload

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

  def combinePayload(nodes: Seq[_ <: WithPayload[P]]): P = {
    strategy.combinePayload(nodes)
  }

  override def toString = {
    val acc = new ArrayBuffer[String]
    root.foreach((d, n) => acc += ("  " * d + n))
    acc.mkString("\n")
  }
}

class EmptyNodeException extends Exception

trait TreeStrategy2[P] {
  def split(nodes: Seq[_ <: WithPayload[P]]): Option[Seq[Seq[_ <: WithPayload[P]]]]
  def findTarget(payload: P, targets: Seq[_ <: WithPayload[P]]): Int
  def combinePayload(nodes: Seq[_ <: WithPayload[P]]): P
}


trait WithPayload[P] {
  def payload: P
}

abstract class Node2[P](val tree: Tree2[P], var children: Seq[_ <: WithPayload[P]] = new Array[WithPayload[P]](0)) extends WithPayload[P] {
  def insert(l: WithPayload[P]): Option[Seq[Seq[_ <: WithPayload[P]]]]

  def payload: P = {
    tree.combinePayload(children);
  }
  
  def foreach(f : (Int, WithPayload[P]) => Unit, depth : Int = 0) : Unit = {
    f(depth, this)
  }

  override def toString = {
    "* : " + payload.toString
  }
}

class LeafNode[P](tree: Tree2[P], readyChildren: Seq[_ <: WithPayload[P]] = new Array[WithPayload[P]](0)) extends Node2(tree, readyChildren) {

  def insert(l: WithPayload[P]): Option[Seq[Seq[_ <: WithPayload[P]]]] = {
    children = children :+ l
    tree.split(children)
  }

  override def foreach(f : (Int, WithPayload[P]) => Unit, depth : Int = 0) : Unit = {
    super.foreach(f, depth)
    children.foreach(f(depth + 1, _))
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

  override def foreach(f : (Int, WithPayload[P]) => Unit, depth : Int = 0) : Unit = {
    super.foreach(f, depth)
    children.foreach(_ match {
      case n : Node2[P] => n.foreach(f, depth + 1)
    })
  }
}