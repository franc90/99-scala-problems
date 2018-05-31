package problems.lists

import org.scalatest._

class ListsTest extends FlatSpec {

  //  -------------- P01 --------------

  "Lists" should "return only last value" in {
    assert(Lists.ultimate(List(1, 2, 3, 4, 5)) === 5)
  }

  it should "return first element if no more in list" in {
    assert(Lists.ultimate(List(123)) === 123)
  }

  it should "throw NoSuchElementException if empty list is supplied" in {
    assertThrows[NoSuchElementException] {
      Lists.ultimate(List())
    }
  }

  //  -------------- P02 --------------

  it should "return penultimate of long list" in {
    assert(Lists.penultimate(List(1, 2, 3, 5, 8)) === 5)
  }

  it should "return penultimate with BuiltIns of long list" in {
    assert(Lists.penultimateWithBuiltin(List(1, 2, 3, 5, 8)) === 5)
  }

  it should "return penultimate in 2 elem list" in {
    assert(Lists.penultimate(List(56, 22)) === 56)
  }

  it should "throw NoSuchElementException for 1 elem list" in {
    assertThrows[NoSuchElementException] {
      Lists.penultimate(List(1))
    }
  }

  it should "throw NoSuchElementException for empty list" in {
    assertThrows[NoSuchElementException] {
      Lists.penultimate(List())
    }
  }

  //  -------------- P03 --------------

  it should "return 3rd element" in {
    assert(Lists.nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }

  it should "return 3rd element via built ins" in {
    assert(Lists.nth2(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }

  //  -------------- P04 --------------

  it should "find len with acc" in {
    assert(Lists.length(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  it should "find len with foldLeft" in {
    assert(Lists.foldLeftLength(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  it should "find len with foldRight" in {
    assert(Lists.foldRightLength(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  it should "find len of empty with acc" in {
    assert(Lists.length(List()) === 0)
  }

  //  -------------- P05 --------------

  it should "foldLeft reverse list" in {
    assert(Lists.foldLeftReverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  }

  it should "foldRight reverse list" in {
    assert(Lists.foldRightReverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  }

  //  -------------- P07 --------------

  it should "flatten list" in {
    assert(Lists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  it should "flatMapFlatten list" in {
    assert(Lists.flatMapFlatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  //  -------------- P08 --------------

  it should "eliminate consecutive duplicates of list" in {
    assert(Lists.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "eliminate consecutive duplicates of list with foldRight" in {
    assert(Lists.foldRightCompress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  //  -------------- P09 --------------

  it should "pack consecutive duplicates of list elements into subLists" in {
    assert(Lists.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    )
  }

  it should "pack consecutive duplicates of list elements into subLists with span" in {
    assert(Lists.packWithSpan(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    )
  }

  //  -------------- P10 --------------

  it should "create run-length encoding of a list" in {
    assert(Lists.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  //  -------------- P11 --------------

  it should "create modified run-len encoding" in {
    assert(Lists.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  //  -------------- P12 --------------

  it should "decode a run-len encoded list" in {
    assert(Lists.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  //  -------------- P13 --------------

  it should "create run-len encoding of list directly" in {
    assert(Lists.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    )
  }

  //  -------------- P14 --------------

  it should "duplicate the elements of a list" in {
    assert(Lists.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  //  -------------- P15 --------------

  it should "duplicate the elements of a list n times" in {
    assert(Lists.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  //  -------------- P16 --------------

  it should "drop every nth element from a list" in {
    assert(Lists.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  //  -------------- P17 --------------

  it should "split a list into two parts" in {
    assert(Lists.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  it should "split a list into two parts #2" in {
    assert(Lists.splitWithRecursion(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  it should "split a list into two parts #3" in {
    assert(Lists.splitWithRecursion2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  //  -------------- P18 --------------

  it should "extract a slice from a list" in {
    assert(Lists.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
  }

  it should "extract a slice from a list #2" in {
    assert(Lists.slice2(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
  }

  //  -------------- P19 --------------

  it should "rotate a list n places to the left #1" in {
    assert(Lists.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  it should "rotate a list n places to the left #2" in {
    assert(Lists.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
  it should "rotate a list n places to the left #3" in {
    assert(Lists.rotate2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  it should "rotate a list n places to the left #4" in {
    assert(Lists.rotate2(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  //  -------------- P20 --------------

  it should "remove kth element from list" in {
    assert(Lists.removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), 'b))
  }

  //  -------------- P21 --------------

  it should "insert an element at given position in list" in {
    assert(Lists.insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  }

  it should "insert an element at given position in list #2" in {
    assert(Lists.insertAt2('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  }

  //  -------------- P22 --------------

  it should "create a list containing all integers within a given range" in {
    assert(Lists.range(4, 9) == List(4, 5, 6, 7, 8, 9))
  }

  //  -------------- P23 --------------

  it should "extract randomly n elements from a list" in {
    assert(Lists.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).length == 3)
  }

  //  -------------- P24 --------------

  it should "extract draw n different numbers from the set 1..max" in {
    val ints = Lists.lotto(6, 49)
    assert(ints.length == 6)
  }

}
