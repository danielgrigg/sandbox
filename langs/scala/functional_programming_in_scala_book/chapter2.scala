object Chapter2 {

  def myfib(n:Int): Int = {
    @annotation.tailrec
    def go(prev:Int, value:Int, n:Int): Int =
      if (n < 1) prev 
      else go(value, value+prev, n-1)
    go(0, 1, n-1)
  }
}
