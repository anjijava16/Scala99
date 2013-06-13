package scala99

//hbaltree
object P99_61 {
  sealed abstract class Tree[+T] {
    def addValue[E >: T](n: E)(implicit f: (E) => Ordered[E]): Tree[E]
    def nodeCount: Int
    def leafCount: Int
    def internalList: List[T]
    def atLevel(n: Int): List[T]
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def addValue[E >: T](n: E)(implicit f: (E) => Ordered[E]): Tree[E] = if (f(n) < value)
      Node(value, left.addValue(n), right) else Node(value, left, right.addValue(n))
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override def nodeCount: Int = 1 + left.nodeCount + right.nodeCount
    def leafCount: Int = (left, right) match {
      case (End, End) => 1
      case (_, _) => left.leafCount + right.leafCount
    }

    def internalList: List[T] = (left, right) match {
      case (End, End) => List[T]()
      case (_, _) => value :: left.internalList ::: right.internalList
    }

    def atLevel(n: Int): List[T] = n match {
      case x if n == 1 => List[T](value)
      case x => left.atLevel(n - 1) ::: right.atLevel(n - 1)
    }

  }
  case object End extends Tree[Nothing] {
    override def toString = "."
    override def addValue[E >: Nothing](n: E)(implicit f: (E) => Ordered[E]): Tree[E] = Node(n)
    override def nodeCount: Int = 0
    override def leafCount: Int = 0
    override def internalList: List[Nothing] = List()
    override def atLevel(n: Int): List[Nothing] = List()
  }

  object Node {
    def apply[T](value: T): Node[T] = new Node(value, End, End)
  }

  object Tree {

    def fromList[E](ls: List[E])(implicit f: (E) => Ordered[E]): Tree[E] = ls.foldLeft(End: Tree[E])((tree, elem) => tree.addValue(elem)(f))

    /**
     * num = number of elements in the tree
     */

  }

  val g = 5                                       //> g  : Int = 5
  val t1 = Tree.fromList(List(1, 2, 3, 4, 5))     //> t1  : scala99.P99_61.Tree[Int] = T(1 . T(2 . T(3 . T(4 . T(5 . .)))))
  t1.internalList                                 //> res0: List[Int] = List(1, 2, 3, 4)
  Node('x', Node('x'), End).leafCount             //> res1: Int = 1
  Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
                                                  //> res2: List[Char] = List(a, c)
  Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)
                                                  //> res3: List[Char] = List(b, c)
}