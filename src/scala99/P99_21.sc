package scala99

//26,27
object P99_21 {
  //21
  def insertAt(t: Any, n: Int, ls: List[Any]) = ls splitAt (n) match {
    case (xs, ys) => xs ::: t :: ys
  }                                               //> insertAt: (t: Any, n: Int, ls: List[Any])List[Any]
  insertAt('new, 1, List('a, 'b, 'c, 'd))         //> res0: List[Any] = List('a, 'new, 'b, 'c, 'd)

  //22
  def range(st: Int, end: Int) = List.make(st, end + 1)
                                                  //> range: (st: Int, end: Int)List[Int]
  range(3, 7)                                     //> res1: List[Int] = List(8, 8, 8)

  //23
  def f(n:Int) = (n * Math.random).toInt          //> f: (n: Int)Int
  def randomSelect(n: Int, ls: List[Any]):List[Any] = {
    for {
      i <- List.make(1, n)
      j = f(ls.size)
    } yield ls(j)
  }                                               //> randomSelect: (n: Int, ls: List[Any])List[Any]
   
  val l = List(1, 2, 3, 4, 5, 6, 7, 8)            //> l  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)
  randomSelect(3, l)                              //> res2: List[Any] = List(4)
  
  //24 n random numbers between 1 to m
  def lotto(n:Int, m:Int):List[Int] = n match {
  	case 0 => List[Int]()
  	case x => f(m) :: lotto(n-1,m)
  }                                               //> lotto: (n: Int, m: Int)List[Int]
  lotto(6,49)                                     //> res3: List[Int] = List(18, 5, 48, 18, 1, 48)
  
  //25
   def permute(ls: List[Any]): List[Any] = {
    var t: Any = null;
    val arr = ls.toArray
    val len = arr.length
    for (i <- 0 until arr.length) {
      val j = (Math.random * (len - i)).toInt + i
      t = arr(i)
      arr.update(i, arr(j))
      arr.update(j, t)
    }
    arr.toList
  }                                               //> permute: (ls: List[Any])List[Any]

  permute(List(1, 2, 3, 4, 5, 6)) //              //> res4: List[Any] = List(3, 2, 5, 6, 1, 4)
  
   //26
  def combinations[T](n: Int, ls: List[T]): List[List[T]] = {
    var ms: List[List[T]] = List[List[T]]();
    val len = ls.size
    if (n > len)
      throw new Error();
    else if (n == len)
      List(ls)
    else if (n == 1)
      ls map (a => List(a))
    else {
      for (i <- n to len) {
        val take: List[T] = ls take i;
        val temp = combinations(n - 1, take.init) map (a => take.last :: a)
        ms = ms ::: temp
      }
      ms
    }
  }                                               //> combinations: [T](n: Int, ls: List[T])List[List[T]]

  combinations(2, List(1, 2, 3))                  //> res5: List[List[Int]] = List(List(2, 1), List(3, 1), List(3, 2))
  combinations(3, List.range(1, 20)).size         //> res6: Int = 969

//27
  def group3[T](ls: List[T]) = {
    for {
      a <- combinations(2, ls)
      b = ls diff a
      c <- combinations(3, b)
    } yield List(a, c, b diff c)
  }                                               //> group3: [T](ls: List[T])List[List[List[T]]]

  group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).size
                                                  //> res7: Int = 1260

  def groupN[T](elem: List[Int], ls: List[T]): List[List[List[T]]] = elem match {
    case Nil => List(Nil)
    case x :: xs => combinations(x, ls) flatMap { a =>
      groupN(xs, ls diff a) map (b => a :: b)
    }
  }                                               //> groupN: [T](elem: List[Int], ls: List[T])List[List[List[T]]]

  groupN(List(2, 2), List('a, 'b, 'c, 'd)).mkString("\n")
                                                  //> res8: String = List(List('b, 'a), List('c, 'd))
                                                  //| List(List('c, 'a), List('b, 'd))
                                                  //| List(List('c, 'b), List('a, 'd))
                                                  //| List(List('d, 'a), List('b, 'c))
                                                  //| List(List('d, 'b), List('a, 'c))
                                                  //| List(List('d, 'c), List('a, 'b))
  
  //28
  def lsort[T](ls: List[List[T]]): List[List[T]] = {
    ls sortWith (_.size < _.size)
  }                                               //> lsort: [T](ls: List[List[T]])List[List[T]]
  lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
                                                  //> res9: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List(
                                                  //| 'm, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))


  def lsortFreq[T](ls: List[List[T]]) = ls.groupBy[Int](a => a.length).toList sortWith((i,j) => i._2.size < j._2.size) flatMap (a => a._2)
                                                  //> lsortFreq: [T](ls: List[List[T]])List[List[T]]
  lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
                                                  //> res10: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, '
                                                  //| b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
  
  
 }