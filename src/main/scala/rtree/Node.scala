package rtree

import scala.collection.mutable.ArrayBuffer

abstract class Node {
  def shape: Shape
  def foreach(f : (Int, Node) => Unit, depth : Int = 0) = { f(depth, this) }
}

class Leaf(val id : Long, val shape: Shape) extends Node {
  
  override def toString = {
    id + " : " + shape
  }
}

class Container(val tree : Tree, val strategy: TreeStrategy, ch : Array[Node]) extends Node with Cache[Shape] {
  
  val children = new ArrayBuffer[Node](ch.length) ++= ch

  override def foreach(f : (Int, Node) => Unit, depth : Int = 0) = { 
    super.foreach(f, depth)
    children.foreach(_.foreach(f, depth + 1))
  }

  def shape = {
    getFromCache match {
      case None => throw new EmptyContainerException
      case Some(r) => r
    }
  }

  def insert(id : Long, shape: Shape) : Unit = {
    clearCache()
    strategy.insertion(children, shape)(tree, id)
    strategy.overflow(children)(tree)
  }

  override def computeCache = {
    children.foldLeft[Option[Shape]](None) { (acc, b) => Option(b.shape + acc.orNull) }
  }

  override def toString = {
    "* : " + shape
  }
}

class EmptyContainerException extends Exception