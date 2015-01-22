package org.smop.typeclass

object ListOps {
  /**
   * The mapAccumL function behaves like a combination of map and foldl; it applies a function to each element of a list, passing an accumulating parameter from left to right, and returning a final value of this accumulator together with the new list.
   */
  def mapAccumL[X, Y, ACC](s: ACC, lx: List[X])(f: (ACC, X) => (ACC, Y)): (ACC, List[Y]) = lx match {
    case Nil => (s, List.empty)
    case x :: xs =>
      val (sp, y) = f(s, x)
      val (spp, ys) = mapAccumL(sp, xs)(f)
      (spp, y :: ys)
  }
}
