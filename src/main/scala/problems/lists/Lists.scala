package problems.lists

import java.util.NoSuchElementException

import scala.util.Random

object Lists {

  def ultimate(list: List[Int]): Int = list match {
    case head :: Nil => head
    case _ :: tail => ultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimate(list: List[Int]): Int = list match {
    case head :: _ :: Nil => head
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimateWithBuiltin(list: List[Int]): Int = {
    if (list.isEmpty) throw new NoSuchElementException
    list.init.last
  }

  def nth(n: Int, list: List[Int]): Int = list match {
    case head :: _ if n == 0 => head
    case _ :: tail => nth(n - 1, tail)
    case _ => throw new NoSuchElementException
  }

  def nth2(n: Int, list: List[Int]): Int =
    if (n >= 0) list(n) else throw new NoSuchElementException

  def length(list: List[Int]): Int = {
    def lengthWithAcc(acc: Int, list: List[Int]): Int = list match {
      case Nil => acc
      case _ :: tail => lengthWithAcc(acc + 1, tail)
    }

    lengthWithAcc(0, list)
  }

  def foldLeftLength(list: List[Int]): Int = list.foldLeft(0) { (len, _) => len + 1 }

  def foldRightLength(list: List[Int]): Int = list.foldRight(0) { (_, len) => len + 1 }

  def foldLeftReverse(list: List[Int]): List[Int] = list.foldLeft(List[Int]()) { (reversed, elem) => elem :: reversed }

  def foldRightReverse(list: List[Int]): List[Int] = list.foldRight(List[Int]()) { (elem, reversed) => reversed :+ elem }

  def flatten(list: List[_]): List[Int] = list match {
    case Nil => List[Int]()
    case (head :: tail) :: mainTail => head.asInstanceOf[Int] :: flatten(tail) ++ flatten(mainTail)
    case head :: tail => head.asInstanceOf[Int] :: flatten(tail)
  }

  def flatMapFlatten(list: List[_]): List[Int] = list flatMap {
    case list: List[_] => flatMapFlatten(list)
    case elem => List(elem.asInstanceOf[Int])
  }

  def compress(list: List[Symbol]): List[Symbol] = {
    def compressR(lastSymbol: Symbol, list: List[Symbol]): List[Symbol] = list match {
      case Nil => List[Symbol]()
      case head :: tail if lastSymbol != head => head :: compressR(head, tail)
      case _ :: tail => compressR(lastSymbol, tail)
    }

    compressR(null, list)
  }

  def foldRightCompress(list: List[Symbol]): List[Symbol] =
    list.foldRight(List[Symbol]()) { (elem, compressed) =>
      if (compressed.nonEmpty && compressed.head == elem)
        compressed
      else
        elem :: compressed
    }

  def pack(list: List[Symbol]): List[List[Symbol]] =
    list.foldRight(List[List[Symbol]]()) { (elem, lst) =>
      if (lst.isEmpty || lst.head.head != elem) List[Symbol](elem) :: lst
      else (elem :: lst.head) :: lst.tail
    }

  def packWithSpan(list: List[Symbol]): List[List[Symbol]] =
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span (_ == list.head)
      if (next.isEmpty) List(packed) else packed :: packWithSpan(next)
    }

  def encode(list: List[Symbol]): List[(Int, Symbol)] = packWithSpan(list) map { lst => (lst.size, lst.head) }

  def encodeModified(list: List[Symbol]): List[Any] = encode(list) map {
    t => if (t._1 == 1) t._2 else t
  }

  def decode(list: List[(Int, Symbol)]): List[Symbol] = list.flatMap { case (i, s) => List.fill(i)(s) }

  def encodeDirect(list: List[Symbol]): List[(Int, Symbol)] =
    if (list.isEmpty) Nil
    else {
      val (vals, rest) = list span {
        _ == list.head
      }
      (vals.length, vals.head) :: encodeDirect(rest)
    }

  def duplicate(list: List[Symbol]): List[Symbol] =
    list.flatMap { e => List(e, e) }

  def duplicateN(n: Int, list: List[Symbol]): List[Symbol] =
    list.flatMap {
      List.fill(n)(_)
    }

  def drop(n: Int, list: List[Symbol]): List[Symbol] =
    for {
      (x, i) <- list.zipWithIndex
      if (i + 1) % n != 0
    } yield x

  def drop2(n: Int, list: List[Symbol]): List[Symbol] =
    list.zipWithIndex filter { p => (p._2 + 1) % n != 0 } map {
      _._1
    }

  def split(len: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) =
    (list.take(len), list.drop(len))

  def splitWithRecursion(len: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    def takeR(len: Int, list: List[Symbol]): List[Symbol] = (len, list) match {
      case (0, _) | (_, Nil) => Nil
      case (n, h :: t) => h :: takeR(n - 1, t)
    }

    def dropR(len: Int, list: List[Symbol]): List[Symbol] = (len, list) match {
      case (_, Nil) => Nil
      case (0, h :: t) => h :: dropR(0, t)
      case (n, _ :: t) => dropR(n - 1, t)
    }

    (takeR(len, list), dropR(len, list))
  }

  def splitWithRecursion2(len: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    def splitR(len: Int, left: List[Symbol], right: List[Symbol]): (List[Symbol], List[Symbol]) = (len, right) match {
      case (_, Nil) => (left, Nil)
      case (0, r) => (left, r)
      case (n, h :: t) => splitR(n - 1, left :+ h, t)
    }

    splitR(len, Nil, list)
  }

  def slice(start: Int, end: Int, list: List[Symbol]): List[Symbol] = {
    def sliceR(idx: Int, list: List[Symbol]): List[Symbol] = list match {
      case Nil => Nil
      case _ :: _ if idx >= end => Nil
      case h :: t if idx >= start => h :: sliceR(idx + 1, t)
      case _ :: t => sliceR(idx + 1, t)
    }

    sliceR(0, list)
  }

  def slice2(start: Int, end: Int, list: List[Symbol]): List[Symbol] = {
    list.splitAt(start)._2.splitAt(end - start)._1
  }

  def rotate(places: Int, list: List[Symbol]): List[Symbol] = {
    if (places > 0) list.drop(places) ++ list.take(places)
    else list.drop(list.length + places) ++ list.take(list.length + places)
  }


  def rotate2(places: Int, list: List[Symbol]): List[Symbol] = {
    def convert(places: Int, list: List[Symbol]): Int = {
      var rotate = places % list.size
      while (rotate < 0) rotate += list.size
      while (rotate >= list.size) rotate -= list.size
      rotate
    }

    def rotateR(places: Int, rotated: List[Symbol], list: List[Symbol]): List[Symbol] = (places, list) match {
      case (_, Nil) => Nil
      case (0, l) => l ++ rotated
      case (n, h :: t) => rotateR(n - 1, rotated :+ h, t)
    }

    rotateR(convert(places, list), Nil, list)
  }

  def removeAt[A](k: Int, list: List[A]): (List[A], A) = {
    def removeAtR(k: Int, processed: List[A], list: List[A]): (List[A], A) = list match {
      case Nil => throw new NoSuchElementException()
      case h :: t if k == 0 => (processed ++ t, h)
      case h :: t => removeAtR(k - 1, processed :+ h, t)
    }

    removeAtR(k, Nil, list)
  }

  def insertAt(s: Symbol, k: Int, list: List[Symbol]): List[Symbol] = {
    val (pre: List[Symbol], post: List[Symbol]) = list.splitAt(k)
    pre ::: s :: post
  }

  def insertAt2(s: Symbol, k: Int, list: List[Symbol]): List[Symbol] = list.splitAt(k) match {
    case (pre, post) => pre ::: s :: post
  }

  def range(s: Int, e: Int): List[Int] = List.range(s, e + 1)

  def randomSelect[A](count: Int, list: List[A]): List[A] = {
    def randomSelectR(count: Int, list: List[A], random: Random): List[A] = (count, list) match {
      case (0, _) => Nil
      case (_, Nil) => throw new NoSuchElementException
      case (n, l) =>
        val (lst, s) = removeAt(random.nextInt(l.size), l)
        s :: randomSelectR(n - 1, lst, random)
    }

    randomSelectR(count, list, new Random)
  }

  def lotto(count: Int, max: Int): List[Int] = {
    randomSelect(count, List.range(1, max + 1))
  }

  def randomPermute[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case l =>
      val (remaining, chosen) = removeAt(new Random().nextInt(l.size), l)
      chosen :: randomPermute(remaining)
  }
}
