package rtree

import org.scalatest._

class ContainerTest extends FlatSpec {

  "Container" should "throw EmptyContainerException when empty and asked form MBR" in {
    val c = new Tree().root
    intercept[EmptyContainerException] {
      c.shape
    }
  }

  "Container" should "compute MBR sanely" in {
    val t = new Tree()
    val c = t.root
    c.insert(0, Shape(1,1))
    assert(c.shape === Shape(1, 1))
    c.insert(1, Shape(-1, -1))
    assert(c.shape === Shape(-1, 1, 1, -1))
    c.insert(2, Shape(2, -7))
    assert(c.shape === Shape(-1, 1, 2, -7))
    c.insert(3, Shape(2, -7))
    c.insert(4, Shape(2, -7))
    assert(c.shape === Shape(-1, 1, 2, -7))
    c.insert(5, Shape(-1000, 1000))
    c.insert(6, Shape(1000, -1000))
    c.insert(7, Shape(0, 0))
    assert(c.shape === Shape(-1000, 1000, 1000, -1000))
    
  }

  "Tree" should "print itself out" in {
    val t = new Tree()
    val c = t.root
    c.insert(0, Shape(1,1))
    c.insert(1, Shape(-1, -1))
    c.insert(2, Shape(2, -7))
    c.insert(3, Shape(2, -7))
    c.insert(4, Shape(2, -7))
    c.insert(7, Shape(0, 0))
    
    println(t)
  }
  
}