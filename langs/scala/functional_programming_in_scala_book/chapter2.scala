object Chapter2 {

  def myfib(n:Int): Int = {
    @annotation.tailrec
    def go(prev:Int, value:Int, n:Int): Int =
      if (n < 1) prev 
      else go(value, value+prev, n-1)
    go(0, 1, n-1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = 
      if (n + 1 >= as.length) true
      else if (!ordered(as(n),as(n+1))) false
      else loop(n+1)
    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a:A => 
      b:B => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C):(A, B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a:A => f(g(a))
  }
}
