package scala99

/**
 * 7 - flatten
 11
 */

object Prob99 {
  //1) last
  def last(a: Seq[Int]) = a.last                  //> last: (a: Seq[Int])Int
  def last1(a: Seq[Int]) = a reduceLeft ((i, j) => j)
                                                  //> last1: (a: Seq[Int])Int
  def last2(a: Seq[Int]): Int = a match {
    case Seq() => throw new Error();
    case x :: Seq() => x
    case x :: xs => last2(xs)
  }                                               //> last2: (a: Seq[Int])Int

  last(List(1, 2, 3))                             //> res0: Int = 3
  last1(List(1, 2, 3))                            //> res1: Int = 3
  last2(List(1, 2, 3))                            //> res2: Int = 3

  //2) Penulatimate
  def penultimate[A](a: List[A]): A = a match {
    case Nil => throw new Error()
    case x :: Nil => throw new Error()
    case x :: xs => (a.drop(a.size - 2).head)
  }                                               //> penultimate: [A](a: List[A])A
  def penultimate2[A](a: List[A]): A = a.init.last//> penultimate2: [A](a: List[A])A

  val list = List(1, 2, 3, 4)                     //> list  : List[Int] = List(1, 2, 3, 4)
  penultimate(list)                               //> res3: Int = 3
  penultimate2(list)                              //> res4: Int = 3

  //3) Find kth element of List
  def nth(k: Int, ls: Seq[Int]): Int = ls(k)      //> nth: (k: Int, ls: Seq[Int])Int
  def nth2(k: Int, ls: Seq[Int]): Int = ls match {
    case Nil => throw new Error("Out of Bound");
    case x if (k == 0) => x.head
    case x :: xs => nth2(k - 1, xs)
  }                                               //> nth2: (k: Int, ls: Seq[Int])Int
  nth(2, list)                                    //> res5: Int = 3
  nth2(2, list)                                   //> res6: Int = 3

  //4) Number of elements in the list
  def len(ls: Seq[Int]): Int = ls.size            //> len: (ls: Seq[Int])Int
  def len2(ls: Seq[Int]): Int = ls.foldLeft(0)((i, j) => i + 1)
                                                  //> len2: (ls: Seq[Int])Int
  len2(list)                                      //> res7: Int = 4

  //5) Reverse a list
  def reverse(ls: Seq[Int]) = ls.reverse          //> reverse: (ls: Seq[Int])Seq[Int]
  def reverse2(ls: List[Int]): List[Int] = ls match {
    case Nil => List()
    case x :: xs => reverse2(xs) ++ List(x)
  }                                               //> reverse2: (ls: List[Int])List[Int]
  def reverse3(ls: Seq[Int]) = ls.foldLeft(List.empty[Int])((i: List[Int], j) => j :: i)
                                                  //> reverse3: (ls: Seq[Int])List[Int]
  def reverse4(ls: Seq[Int]) = {
    var xs = List.empty[Int]
    for (x <- ls)
      xs = x :: xs;

    xs

  }                                               //> reverse4: (ls: Seq[Int])List[Int]

  reverse2(list)                                  //> res8: List[Int] = List(4, 3, 2, 1)
  reverse3(list)                                  //> res9: List[Int] = List(4, 3, 2, 1)
  reverse4(list)                                  //> res10: List[Int] = List(4, 3, 2, 1)

  //6) Find out whether a list is a palindrome.
  def palindrome[A](ls: List[A]) = {
    val size = ls.size - 1
    val k = for {
      i <- 0 to size / 2
      if (ls(i) != ls(size - i))
    } yield false

    if (k.size == 0)
      true else false

  }                                               //> palindrome: [A](ls: List[A])Boolean

  def palindrome2[A](ls: List[A]) = ls == ls.reverse
                                                  //> palindrome2: [A](ls: List[A])Boolean

  palindrome(List(1, 2, 1))                       //> res11: Boolean = true
  palindrome(List(1, 2, 2, 1))                    //> res12: Boolean = true
  palindrome(List(1, 2, 3, 1))                    //> res13: Boolean = false

  //7***) flatten a list
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }                                               //> flatten: (ls: List[Any])List[Any]

  flatten(List(List(1, 1), List(2), List(3, List(5, 8))))
                                                  //> res14: List[Any] = List(1, 1, 2, 3, 5, 8)

  val k = List(1, List(2, 3), List(List(List(List(4)), List(5)), List(6, 7)), 8)
                                                  //> k  : List[Any] = List(1, List(2, 3), List(List(List(List(4)), List(5)), Lis
                                                  //| t(6, 7)), 8)
  flatten(k)                                      //> res15: List[Any] = List(1, 2, 3, 4, 5, 6, 7, 8)

  //8 Eliminate consecutive duplicates of list elements.
  def compress[A](ls: List[Any]) = {
    var prev: Any = null;
    var ans: List[Any] = List.empty[Any]
    for (a <- ls) {
      if (prev != a)
        ans = a :: ans
      prev = a
    }
    ans.reverse
  }                                               //> compress: [A](ls: List[Any])List[Any]

  def compress2[A](ls: List[Any]): List[Any] = ls match {
    case List() => List()
    case h :: tail => h :: compress2(tail.dropWhile(p => p == h))
  }                                               //> compress2: [A](ls: List[Any])List[Any]

  //if u use foldRight, then no need to reverse
  def compress3[A](ls: List[Any]): List[Any] = (ls foldLeft (List.empty[Any])) { (h, r) =>
    if (h.isEmpty)
      List(r)
    else if (h.head == r) h
    else r :: h
  }.reverse                                       //> compress3: [A](ls: List[Any])List[Any]
  val l8 = List('a', 'b', 'b', 'b', 'b', 'b', 'd', 'd', 'd', 'a')
                                                  //> l8  : List[Char] = List(a, b, b, b, b, b, d, d, d, a)
  compress2(l8)                                   //> res16: List[Any] = List(a, b, d, a)
  compress3(l8)                                   //> res17: List[Any] = List(a, b, d, a)

  //9 pack
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls isEmpty) List();
    else {
      val xs = ls span (x => x == ls.head)
      xs._1 :: pack(xs._2)
    }
  }                                               //> pack: [A](ls: List[A])List[List[A]]
  pack(l8)                                        //> res18: List[List[Char]] = List(List(a), List(b, b, b, b, b), List(d, d, d),
                                                  //|  List(a))
  pack(List(1, 1, 1, 2))                          //> res19: List[List[Int]] = List(List(1, 1, 1), List(2))

  //10 encode
  def encode[T](ls: List[T]): List[(Int, T)] = pack(ls) map (a => (a.size, a.head))
                                                  //> encode: [T](ls: List[T])List[(Int, T)]
  encode(l8)                                      //> res20: List[(Int, Char)] = List((1,a), (5,b), (3,d), (1,a))
  encode(List())                                  //> res21: List[(Int, Nothing)] = List()

  //11 encodeModified -------------------------------------------------------------------------------------
  def encodeModified[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }
                                                  //> encodeModified: [A](ls: List[A])List[Either[A,(Int, A)]]
  encodeModified(l8)                              //> res22: List[Either[Char,(Int, Char)]] = List(Left(a), Right((5,b)), Right((
                                                  //| 3,d)), Left(a))

  //12 -------------------------------------------------------------------------------------
  def decode[T](ls: List[(Int, T)]): List[T] = ls flatMap (a => Seq.fill(a._1)(a._2))
                                                  //> decode: [T](ls: List[(Int, T)])List[T]
  decode(encode(l8)) == l8                        //> res23: Boolean = true

  //13-------------------------------------------------------------------------------------

  def encodeDirect[T](ls: List[T]): List[(Int, T)] = {
    if (ls isEmpty) List();
    else {
      val xs = ls span (x => x == ls.head)
      (xs._1.size, xs._1.head) :: encodeDirect(xs._2)
    }
  }                                               //> encodeDirect: [T](ls: List[T])List[(Int, T)]
  encodeDirect(l8) == encode(l8)                  //> res24: Boolean = true


}