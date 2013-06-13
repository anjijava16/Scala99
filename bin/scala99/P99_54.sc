package scala99

//hbaltree
object P99_55 {
  sealed abstract class Tree[+T] {
    def isSymmetric: Boolean
    def isMirror[V](t: Tree[V]): Boolean
    def addValue[E >: T](n: E)(implicit f: (E) => Ordered[E]): Tree[E]

    def nodeCount: Int
    def leafCount: Int
    def internalList:List[T]
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def isSymmetric: Boolean = left isMirror (right)
    override def isMirror[E](two: Tree[E]): Boolean = two match {
      case t: Node[E] => (left isMirror (t.left)) && (right isMirror (t.right))
      case _ => this == End
    }
    override def addValue[E >: T](n: E)(implicit f: (E) => Ordered[E]): Tree[E] = if (f(n) < value)
      Node(value, left.addValue(n), right) else Node(value, left, right.addValue(n))
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override def nodeCount: Int = 1 + left.nodeCount + right.nodeCount
    def leafCount: Int = (left, right) match {
      case (End, End) => 1
      case (_, _) => left.leafCount + right.leafCount
    }
    
    def internalList:List[T] = (left, right) match {
      case (End, End) => List[T]()
      case (_, _) => value :: left.internalList ::: right.internalList
    }
     
  }
  case object End extends Tree[Nothing] {
    override def toString = "."
    override def isSymmetric = true
    override def isMirror[V](t: Tree[V]) = t == End
    override def addValue[E >: Nothing](n: E)(implicit f: (E) => Ordered[E]): Tree[E] = Node(n)
    override def nodeCount: Int = 0
    override def leafCount: Int = 0
    override def internalList:List[Nothing] = List()
  }

  object Node {
    def apply[T](value: T): Node[T] = new Node(value, End, End)
  }

  object Tree {
    def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
      case n if (nodes == 0) => List[Tree[T]](End)
      case n if (nodes == 1) => List[Node[T]](Node(value, End, End))
      case n if (nodes % 2 == 0) => {
        val i = cBalanced((n - 1) / 2, value)
        val j = cBalanced((n) / 2, value)
        val c = for {
          a <- i
          b <- j
        } yield List(new Node(value, a, b), new Node(value, b, a))

        c flatten
      }
      case n if (n % 2 == 1) => {
        val i = cBalanced((nodes) / 2, value)
        val j = cBalanced((nodes) / 2, value)
        for {
          a <- i
          b <- j
        } yield new Node(value, a, b)
      }
    }

    def fromList[E](ls: List[E])(implicit f: (E) => Ordered[E]): Tree[E] = ls.foldLeft(End: Tree[E])((tree, elem) => tree.addValue(elem)(f))
    /**
     * num = number of elements in the tree
     */
    def symmetricBalancedTrees[E](num: Int, elem: E): List[Tree[E]] = cBalanced(num, elem) filter (_ isSymmetric)

    def hBalTrees[E](height: Int, elem: E): List[Tree[E]] = height match {
      case x if (height == 0) => List(End)
      case x if (height == 1) => List(Node(elem))
      case x if (height >= 1) => {
        val full = hBalTrees(height - 1, elem)
        val short = (hBalTrees(height - 2, elem))
        full.flatMap(a => full map (b => Node(elem, a, b))) ::: full.flatMap(a => short.flatMap(b => List(Node(elem, a, b), Node(elem, b, a))))
      }
    }

    def minHbalNodes(height: Int): Int = height match {
      case x if height == 0 => 0
      case x if height == 1 => 1
      case x if height == 2 => 2
      case x => 1 + minHbalNodes(x - 1) + minHbalNodes(x - 2)
    }

    def maxHbalHeight(nodes: Int): Int = Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last
    def minHbalHeight(nodes: Int): Int = (1 + math.log10(nodes) / math.log10(2)).toInt

    def hbalTreesWithNodes[T](nodes: Int, e: T) = (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hBalTrees(_, e)).filter(_.nodeCount == nodes).toList
  }

  val g = 5                                       //> g  : Int = 5
  Tree.cBalanced(g, "x").size                     //> res0: Int = 4

  Tree.cBalanced(g, "x").mkString("\n")           //> res1: String = T(x T(x . T(x . .)) T(x . T(x . .)))
                                                  //| T(x T(x . T(x . .)) T(x T(x . .) .))
                                                  //| T(x T(x T(x . .) .) T(x . T(x . .)))
                                                  //| T(x T(x T(x . .) .) T(x T(x . .) .))
  Node('a', Node('b'), Node('c')).isSymmetric     //> res2: Boolean = true
  Node('a', Node('b'), Node('c', Node('d), Node('e))).isSymmetric
                                                  //> res3: Boolean = false
  val t1 = Tree.fromList(List(1, 2, 3, 4, 5))     //> t1  : scala99.P99_55.Tree[Int] = T(1 . T(2 . T(3 . T(4 . T(5 . .)))))
  t1.internalList                                 //> res4: List[Int] = List(1, 2, 3, 4)
  Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric == true
                                                  //> res5: Boolean = true
  Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric == false
                                                  //> res6: Boolean = true
  Tree.symmetricBalancedTrees(5, "x").mkString("\n")
                                                  //> res7: String = T(x T(x . T(x . .)) T(x . T(x . .)))
                                                  //| T(x T(x T(x . .) .) T(x T(x . .) .))
  //val check = Node("1", Node("1", Node("1", Node("1", End, End), End, End), Node("1", End, End), Node("1", Node("1", End, End), End)))
  Tree.cBalanced(5, "x").size                     //> res8: Int = 4
  Tree.hBalTrees(5, "1").size                     //> res9: Int = 108675
  Tree.minHbalNodes(3)                            //> res10: Int = 4
  Tree.maxHbalHeight(4)                           //> res11: Int = 3
  Tree.hbalTreesWithNodes(15, 'a').size           //> res12: Int = 1553
  Node('x', Node('x'), End).leafCount             //> res13: Int = 1
  Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
                                                  //> res14: List[Char] = List(a, c)
}