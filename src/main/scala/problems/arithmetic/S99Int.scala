package problems.arithmetic {

  class S99Int(val start: Int) {

    import S99Int._

    def isCoprimeTo(value: Int): Boolean = gcd(start, value) == 1

    def totient: Int = {
      val ints = for {
        value <- 1 until start
        if start.isCoprimeTo(value)
      } yield value
      ints.size
    }

    def betterTotient: Int = 1 until start count {
      start.isCoprimeTo(_)
    }

  }

  object S99Int {
    private[arithmetic] implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)


  }

}