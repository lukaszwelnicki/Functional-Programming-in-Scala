package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == r || c < 1) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def parenthesesBalanced(openCount: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) openCount == 0
      else if (chars.head.equals('(')) parenthesesBalanced(openCount + 1, chars.tail)
      else if (chars.head.equals(')')) parenthesesBalanced(openCount - 1, chars.tail)
      else parenthesesBalanced(openCount, chars.tail)
    }
    parenthesesBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) 0
      else if (money == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
