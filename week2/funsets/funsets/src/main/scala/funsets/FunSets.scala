package funsets

import com.sun.xml.internal.ws.api.streaming.XMLStreamReaderFactory.Default


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean


  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.l
   */
    def singletonSet(elem: Int): Set = { x => x == elem }
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */


    def union(s: Set, t: Set): Set = {
      elem =>  (contains(s,elem) || contains(t,elem))
    }
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = {
    elem =>  (contains(s,elem) && contains(t,elem))
  }
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = {
    elem =>  (contains(s,elem) && !contains(t,elem))
  }
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = {
      elem => p(elem) && contains(s,elem)
  }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound) true  // sortie de la fonction de recursion
        else if (contains(s,a) && !p(a)) false
        else iter(a+1) // aller vers l'element suivant
      }
      iter(-bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound) false  // sortie de la fonction de recursion
        else if (contains(s,a) && p(a)) true
        else iter(a+1) // aller vers l'element suivant
      }
      iter(-bound)
  }


  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = {
      // for each element in the set
      // apply the function f
      // save the result in a new set

      var resultSet:Set = singletonSet(-bound-1)
      val initialEmptySet:Set = singletonSet(-bound-1)

      for (i <- -bound to bound if contains(s, i)) {
        val b = f(i)
        resultSet = union(resultSet,singletonSet(b))
      }
      diff(resultSet,initialEmptySet)

  }
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
