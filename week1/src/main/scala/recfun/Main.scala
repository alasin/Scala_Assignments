package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val cur = 0

    def checkBal(chars: List[Char], cur: Int): Boolean =
      if (cur == 0) {
        if (chars.isEmpty)

          true
        else if (chars.head == '(')
          checkBal(chars.tail, cur + 1)
        else if (chars.head == ')')
          false
        else
          checkBal(chars.tail, cur)
      } else {
        if (chars.isEmpty) false
        else if (chars.head == ')')
          checkBal(chars.tail, cur - 1)
        else if (chars.head == '(')
          checkBal(chars.tail, cur + 1)
        else
          checkBal(chars.tail, cur)
      }

    if (chars.head == '(') checkBal(chars.tail, cur + 1)
    else if (chars.head == ')') false
    else if (chars.isEmpty) true
    else checkBal(chars.tail, cur)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0) 0
    else if (money > 0 && coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

}
