package map.base

import org.scalatest.FlatSpec

class ShapeTest extends FlatSpec {

  "Shape" should "expand sanely" in {
    val shape = Shape(1, 1)

    assert(shape + null == shape)
    assert(shape + shape == shape)

    assert(shape + Shape(-1, -1) == Shape(1, 1, -1, -1))
    assert(shape + Shape(-1, -1) == Shape(-1, 1, 1, -1))
    assert(shape + Shape(-1, -1) + Shape(2, -7) == Shape(-1, 1, 2, -7))
    assert(shape + Shape(-1, -1) + Shape(2, -7) + Shape(0, 0) == Shape(-1, 1, 2, -7))
    assert(shape
      + Shape(-1, -1)
      + Shape(2, -7)
      + Shape(0, 0)
      + Shape(-1000, 1000)
      + Shape(1000, -1000)
      == Shape(-1000, 1000, 1000, -1000))

    assert(Shape(1, 1, 2, 2) + null == Shape(1, 1, 2, 2))
    assert(Shape(1, 1, 2, 2) + Shape(1, 1, 2, 2) == Shape(1, 1, 2, 2))
  }

  "Shape" should "report contains" in {
    assert(Shape(0, 0).contains(Shape(0, 0)))
    assert(!Shape(0, 0).contains(Shape(1, 1)))

    assert(Shape(10, 10, -10, -10).contains(Shape(0, 0)))
    assert(!Shape(0, 0).contains(Shape(10, 10, -10, -10)))

    assert(Shape(10, 10, -10, -10).contains(Shape(1, 1, -1, -1)))
    assert(!Shape(1, 1, -1, -1).contains(Shape(10, 10, -10, -10)))
    assert(Shape(10, 10, -10, -10).contains(Shape(10, 10, -10, -10)))
    assert(!Shape(10, 10, -10, -10).contains(Shape(25, 1, 26, 2)))

    assert(!Shape(10, 10, -10, -10).contains(Shape(5, 5, 12, 12)))
    assert(!Shape(5, 5, 12, 12).contains(Shape(10, 10, -10, -10)))
  }

  "Shape" should "report intersects" in {
    assert(Shape(0, 0).intersects(Shape(0, 0)))
    assert(!Shape(0, 0).intersects(Shape(1, 1)))

    assert(Shape(10, 10, -10, -10).intersects(Shape(0, 0)))
    assert(Shape(0, 0).intersects(Shape(10, 10, -10, -10)))

    assert(Shape(10, 10, -10, -10).intersects(Shape(1, 1, -1, -1)))
    assert(Shape(1, 1, -1, -1).intersects(Shape(10, 10, -10, -10)))
    assert(Shape(10, 10, -10, -10).intersects(Shape(10, 10, -10, -10)))
    assert(!Shape(10, 10, -10, -10).intersects(Shape(25, 1, 26, 2)))

    assert(Shape(10, 10, -10, -10).intersects(Shape(5, 5, 12, 12)))
    assert(Shape(5, 5, 12, 12).intersects(Shape(10, 10, -10, -10)))

  }

}