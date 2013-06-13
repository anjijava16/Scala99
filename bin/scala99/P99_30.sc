package scala99
object P99_30 {
  class S99Int(val start: Int) {
    import S99Int._
    def isPrime: Boolean = if(start ==1) true else primes takeWhile (x => x <= start) contains (start)

    def gcd(v: Int): Int = {
      def gcdRecur(a: Int, b: Int): Int =
        if (b == 0) a else gcdRecur(b, a % b)
      gcdRecur(start, v)
    }

    def isCoprimeTo(b: Int): Boolean = gcd(b) == 1

    def totient: Int = (1 to start).foldLeft(0)((f, num) => if (isCoprimeTo(num)) f + 1 else f)

    def primeFactors: List[Int] = {
      def pr(st: Int, ans: List[Int], ps: Stream[Int]): List[Int] = if (st.isPrime)
        st :: ans
      else if (st % ps.head == 0) pr(st / ps.head, ps.head :: ans, ps)
      else pr(st, ans, ps.tail)
     
      pr(start, List[Int](), primes)
    }
    
    def primeFactorMultiplicity:List[(Int,Int)] = start.primeFactors groupBy(x => x) map (a => (a._1, a._2.size)) toList
    
    def phi:Int = primeFactorMultiplicity.foldLeft(1)((a:Int, b) => a * (b._1 -1) * Math.pow(b._1, b._2-1).toInt)
    
    def goldbach:(Int,Int) = primes takeWhile(x => x < start) find (y => (start-y).isPrime) match {
    case None => throw new Error()
    case m:Some[Int] => (start - m.get,m.get)
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    val primes: Stream[Int] = Stream.cons(2, Stream.from(3, 2) filter { x => primes takeWhile (_ <= x / 2) forall (x % _ != 0) })
  }

  import S99Int._
  //31
  315 isPrime;                                    //> res0: Boolean = false
  println(List(1, 2))                             //> List(1, 2)
  36.gcd(63)                                      //> res1: Int = 9

  35.isCoprimeTo(64)                              //> res2: Boolean = true
  10090.totient                                   //> res3: Int = 4032
  315.primeFactors                                //> res4: List[Int] = List(7, 5, 3, 3)
  319.primeFactorMultiplicity                     //> res5: List[(Int, Int)] = List((11,1), (29,1))
  10090.phi                                       //> res6: Int = 4032
  10090.totient == 10090.phi                      //> res7: Boolean = true
  
  def listPrimesinRange(x:Range): List[Int] = primes.dropWhile(_ < x.start).takeWhile (_ <=x.end).toList
                                                  //> listPrimesinRange: (x: Range)List[Int]
  listPrimesinRange(7 to 31)                      //> res8: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
  28.goldbach                                     //> res9: (Int, Int) = (23,5)
  
  def printGoldbachList(x:Range){
      for(i <- x){
          if(i%2 ==0)
              println(i.goldbach)
      }
  }                                               //> printGoldbachList: (x: Range)Unit
  printGoldbachList(9 to 20)                      //> (7,3)
                                                  //| (7,5)
                                                  //| (11,3)
                                                  //| (13,3)
                                                  //| (13,5)
                                                  //| (17,3)
           
}