package rtree;

trait Cache[T] {

  var cache: Option[T] = None

  protected def computeCache(): Option[T]

  protected def getFromCache: Option[T] = {
    if (cache == None) {
      cache = computeCache()
    }
    cache
  }

  protected def clearCache() = cache = None
}
