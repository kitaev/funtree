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


}