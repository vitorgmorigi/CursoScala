package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean // Set é um alias de uma função anônima que pede um inteiro e retorna um boolean

  /**
  * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem) // retorna true se um certo elemento contém em um certo conjunto

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = { // método equivalente ao getInstance do Java

      (x: Int) => x == elem // se x for igual ao elemento, retorna true

    }
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = {

      (x: Int) => contains(s, x) || contains(t, x) // tudo que estiver em s ou t retornará true

    }
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = {

      (x: Int) => contains(s, x) && contains(t, x) // só retorna true quando o elemento estiver tanto em s quanto em t

    }
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = {

      (x: Int) => contains(s, x) && !contains(t, x) // só retorna true quando o elemento está apenas em s e não em t

    }
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = {

      (x: Int) => s(x) && p(x) // só retorna o que estiver em p, que por consequencia, tem que estar em s também

    }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = { // s é um conjunto e p é uma regra
    def iter(a: Int): Boolean = {
      if (a > bound) true // se todos os elementos de s corresponderem a regra, retornará true
      else if (contains(s, a) && !p(a)) false // se algum elemento de s nao corresponder a regra, retornará falso
      else iter(a+1) // pula para o próximo elemento de s
    }
    iter(-bound) // começa a verificar de -1000 e termina a verificação em 1000
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound) false // se todos os elementos de s corresponderem a regra, retornará true
        else if (contains(s, a) && p(a)) true // se algum elemento de s nao corresponder a regra, retornará falso
        else iter(a+1) // pula para o próximo elemento de s
      }
      iter(-bound) // começa a verificar de -1000 e termina a verificação em 1000


    }
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = ???
  
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
