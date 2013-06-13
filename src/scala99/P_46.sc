package scala99

object P_46 {
  class S99Logic(a: Boolean) {
    def and(b: Boolean): Boolean = a & b
  }

  object S99Logic {
    implicit def booleanExtra(a: Boolean): S99Logic = new S99Logic(a)
    def and(a: Boolean, b: Boolean): Boolean = a & b
    def or(a: Boolean, b: Boolean): Boolean = a | b
    def nand(a: Boolean, b: Boolean): Boolean = !and(a, b)
    def nor(a: Boolean, b: Boolean): Boolean = !or(a, b)
    def xor(a: Boolean, b: Boolean): Boolean = !(a == b)
  }

  import S99Logic._
  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A	B	=	Results");
    for (i <- List(true, false))
      for (j <- List(true, false))
        println(i + "	" + j + "	" + f(i, j))

  }                                               //> table2: (f: (Boolean, Boolean) => Boolean)Unit

  true and false                                  //> res0: Boolean = false

  def gray(n: Int): List[String] = {
    def grayR(x: Int, ls: List[String]): List[String] = x match {
      case 1 => ls
      case a => {
      	grayR(a-1,ls map ( i => "0"+i)) :::
      	grayR(a-1,ls map ( i => "1"+i))
      }
    }
    grayR(n, List[String]("0","1"))
  }                                               //> gray: (n: Int)List[String]
	gray(4)                                   //> res1: List[String] = List(0000, 0001, 1000, 1001, 0100, 0101, 1100, 1101, 0
                                                  //| 010, 0011, 1010, 1011, 0110, 0111, 1110, 1111)
}