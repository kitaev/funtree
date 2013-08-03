package map.base

import org.scalatest.FlatSpec

class ShapeTreeTest extends FlatSpec {

  "ShapeTree" should "work" in {
    val tree = new ShapeTree(new ShapeRTreeStrategy(4))

    tree.insert(ShapeTestItem(Shape(0, 0)));
    tree.insert(ShapeTestItem(Shape(1, 1)));
    tree.insert(ShapeTestItem(Shape(-1, -1)));
    tree.insert(ShapeTestItem(Shape(0.5, 0.5)));
    tree.insert(ShapeTestItem(Shape(2, -7)));
    tree.insert(ShapeTestItem(Shape(2, -7)));

    println(tree)
    assert(tree.payload == Shape(-1, 1, 2, -7))
  }

  "ShapeTree" should "work with more number of nodes" in {
    val tree = new ShapeTree(new ShapeRTreeStrategy(10))

    for (x <- 0 to 100) {
      tree.insert(ShapeTestItem(Shape(x, x)));
    }
    println(tree)
    assert(tree.payload == Shape(0, 0, 100, 100))
  }

}