package problems.arithmetic

import org.scalatest.FlatSpec

class S99IntTest extends FlatSpec {

  import problems.arithmetic.S99Int.int2S99Int

  it should "determine the greatest common divisor" in {
    assert(S99Int.gcd(36, 63) == 9)
  }

  it should "determine whether two positive integer numbers are coprime" in {
    assert(35.isCoprimeTo(64))
  }

  it should "calc euler's totient function phi(m)" in {
    assert(10.totient == 4)
  }

  it should "calc euler's totient function phi(m) #2" in {
    assert(10.betterTotient == 4)
  }

}
