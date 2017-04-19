package recfun

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
  def pascal(c: Int, r: Int): Int = if (c == 0) 1 else if (r == 0) 0 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def parenthesisCount(x: List[Char], pCount: Int): Boolean = {
      if (x.isEmpty) pCount == 0
      else if (pCount < 0) false
      else if (x.head == '(') parenthesisCount(x.tail, pCount + 1)
      else if (x.head == ')') parenthesisCount(x.tail, pCount - 1)
      else parenthesisCount(x.tail, pCount)
    }
    parenthesisCount(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeSorted(m: Int, c: List[Int]): Int = {
      if (c.isEmpty) 0
      else if (m < c.min) 0
      else if (m == c.min) 1
      else {
        val h = c.head
        countChangeSorted(m - h, c) + countChange(m, c.tail)
      }
    }
    countChangeSorted(money, coins.sortWith(_<_))
  }
}
