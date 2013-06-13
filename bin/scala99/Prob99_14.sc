package scala99

object Prob99_14 {

  val l14 = List('a, 'b, 'c, 'c, 'd)              //> l14  : List[Symbol] = List('a, 'b, 'c, 'c, 'd)

  //14
  def duplicate(ls: List[Any]) = ls flatMap (a => Seq.fill(2)(a))
                                                  //> duplicate: (ls: List[Any])List[Any]
  duplicate(l14)                                  //> res0: List[Any] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  //15
  def duplicateN(n: Int, ls: List[Any]) = ls flatMap (a => List.make(n, a))
                                                  //> duplicateN: (n: Int, ls: List[Any])List[Any]
  duplicateN(3, l14)                              //> res1: List[Any] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, '
                                                  //| d, 'd)

  //16
  def dropN(n: Int, ls: List[Any]): List[Any] = n match {
    case 0 => throw new Error();
    case x if (ls.size < n) => ls
    case x => ls.take(n - 1) ::: dropN(n, ls.drop(n))
  }                                               //> dropN: (n: Int, ls: List[Any])List[Any]
  dropN(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res2: List[Any] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

  //17
  def split(n: Int, ls: List[Any]) = ls splitAt n //> split: (n: Int, ls: List[Any])(List[Any], List[Any])

  def split2(n: Int, ls: List[Any]): (List[Any], List[Any]) = {
    if (n >= ls.size) (ls, List())
    else if (n < 0) throw new Error()
    else
      n match {
        case -1 => throw new Error()
        case 0 => (List(), ls)
        case x => {
          val k = split2(n - 1, ls.tail)
          (ls.head :: k._1, k._2)
        }
      }
  }                                               //> split2: (n: Int, ls: List[Any])(List[Any], List[Any])
  split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res3: (List[Any], List[Any]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i
                                                  //| , 'j, 'k))
  split2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res4: (List[Any], List[Any]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, '
                                                  //| i, 'j, 'k))
  split2(1, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res5: (List[Any], List[Any]) = (List('a),List('b, 'c, 'd, 'e, 'f, 'g, 'h, '
                                                  //| i, 'j, 'k))
  split2(300, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res6: (List[Any], List[Any]) = (List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j
                                                  //| , 'k),List())
  split2(0, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res7: (List[Any], List[Any]) = (List(),List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h,
                                                  //|  'i, 'j, 'k))
  //18
  def slice(i: Int, k: Int, ls: List[Any]): List[Any] = ls.drop(i).take(k - i)
                                                  //> slice: (i: Int, k: Int, ls: List[Any])List[Any]
  def slice2(i: Int, k: Int, ls: List[Any]): List[Any] = (i, k, ls) match {
    case (0, 0, xs) => List()
    case (_, kk, _) if (kk <= 0) => List()
    case (0, kk, xs) => xs.head :: slice2(0, kk - 1, xs.tail)
    case (ii, kk, xs) => slice2(ii - 1, kk - 1, xs.tail)
  }                                               //> slice2: (i: Int, k: Int, ls: List[Any])List[Any]
  slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res8: List[Any] = List('d, 'e, 'f, 'g)
  slice2(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res9: List[Any] = List('d, 'e, 'f, 'g)

  //19
  def rotate(n: Int, ls: List[Any]): List[Any] = n match {
    case 0 => ls
    case x if (x > 0) => rotate(x - 1, ls.tail ++ List(ls.head))
    case x if (x < 0) => rotate(x + 1, List(ls.last) ++ ls.init)
  }                                               //> rotate: (n: Int, ls: List[Any])List[Any]
  rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res10: List[Any] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res11: List[Any] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 
 //20
 def removeAt(n:Int, ls:List[Any]):(List[Any],Any) = (ls.take(n) ::: ls.drop(n+1),ls(n))
                                                  //> removeAt: (n: Int, ls: List[Any])(List[Any], Any)
removeAt(1, List('a, 'b, 'c, 'd))                 //> res12: (List[Any], Any) = (List('a, 'c, 'd),'b)

}