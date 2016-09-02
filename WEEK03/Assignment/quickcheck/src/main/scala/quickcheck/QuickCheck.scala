package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import quickcheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  //--------------------------------------------------------------------------------
  // Generator of heap type H.
  //--------------------------------------------------------------------------------
  def gen(level: Int): Gen[H] = for {
    i <- arbitrary[A]
    h <- oneOf(const(this.empty), gen(level + 1))
  } yield insert(i, h)

  lazy val genHeap: Gen[H] = gen(0)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //--------------------------------------------------------------------------------
  // ScalaCheck properties to be true.
  //--------------------------------------------------------------------------------
  property("min") = forAll(genHeap) { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("empty") = {
    isEmpty(deleteMin(insert(1, empty)))
  }

  property("min of two") = {
    findMin(insert(2, insert(1, empty))) == 1
  }

  property("findMin(h) <= findMin(deleteMin(h))") = forAll(genHeap) { (h: H) =>
    if (isEmpty(h)) false
    else {
      if (isEmpty(deleteMin(h))) true
      else findMin(deleteMin(h)) >= findMin(h)
    }
  }

  property("meld") = forAll(genHeap, genHeap) {
    (f: H, s: H) => findMin(meld(f, s)) == Math.min(findMin(f), findMin(s))
  }

  property("First min is the minimam of all") = forAll(genHeap) {
    (h: H) =>
      if (isEmpty(h)) false
      else {
        val theMin = findMin(h)
        assureMin(theMin, deleteMin(h))
      }
  }
  def assureMin(theMin: Int, rest: H): Boolean = {
    if (isEmpty(rest)) true
    else {
      if (findMin(rest) < theMin) false
      else assureMin(theMin, deleteMin(rest))
    }
  }
  /*
  property("sorted") = forAll(genHeap) {
    (h: H) =>
      if (isEmpty(h)) false
      else checkSorted(findMin(h), deleteMin(h))
  }
  def checkSorted(prev: Int, h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val cur = findMin(h)
      if (prev > cur) false
      else checkSorted(cur, deleteMin(h))
    }
  }
	*/
  property("sorted") = {
    val h = ((0 until 1000).toList).foldLeft(empty)((heap, i) => insert(i, heap))
    checkAll(0, h)
  }
  def checkAll(min: Int, heap: H): Boolean = {
    if (isEmpty(heap)) true
    else {
      if (min != findMin(heap)) false
      else checkAll(min + 1, deleteMin(heap))
    }
  }
}
