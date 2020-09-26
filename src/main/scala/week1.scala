object week1 {
  def pascal(c: Int, r: Int): Int = {
    if (r == c || c==0 || r== 0) 1
    else pascal(c-1, r-1) + pascal(c , r-1)
  }
  def balance(chars: List[Char]): Boolean = {
    def balance_in(chars: List[Char], num:Int): Int  = {
      if(chars.nonEmpty){
        if (chars.head == '(') balance_in(chars.tail, num + 1)
        else if (chars.head == ')') balance_in(chars.tail, num - 1)
        else balance_in(chars.tail, num)
      }
      else num
    }
    balance_in(chars,0) == 0
  }
  /*
    def countChange(money: Int, coins: List[Int], coins_count:List[Int]): Int = {
      if(money == 0 && coins_count.forall(x => x == 0)) 1
      else if(money > 0 && coins.nonEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0
    }
  */
}
