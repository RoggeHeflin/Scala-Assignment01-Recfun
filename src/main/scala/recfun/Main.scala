package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {

    def factorial(n: Int): Int = {
      @tailrec
      def loop(acc: Int, n: Int): Int =
        if (n == 0) acc
        else loop(acc * n, n - 1)

      loop(1, n)
    }

    factorial(r) / factorial(c) / factorial(r - c)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    var acc: Int = 0

    @tailrec
    def loop(chars: List[Char]): Int = {

      if (chars.head.toString == "(") acc = acc + 1
      if (chars.head.toString == ")") acc = acc - 1

      if ((acc < 0) || chars.tail.isEmpty)
        acc
      else
        loop(chars.tail)
    }

    loop(chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (!(money > 0 && coins.nonEmpty))
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
