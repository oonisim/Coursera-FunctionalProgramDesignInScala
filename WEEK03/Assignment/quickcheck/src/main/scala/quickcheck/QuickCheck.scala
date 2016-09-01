package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import quickcheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //--------------------------------------------------------------------------------
  // Create a heap tree 
  //--------------------------------------------------------------------------------
  def gen(level: Int): Gen[H] = for {
    i <- arbitrary[A]
    //i <- arbitrary[A] if (i > Int.MinValue)
    //i <- arbitrary[A] if (0 && i < Int.MaxValue) // Causes Gave up after 3 successful property evaluations. 98 evaluations were discarded.
    //i <- arbitrary[A] if (Int.MinValue < i && i < Int.MaxValue)
    h <- oneOf(const(this.empty), gen(level + 1))
  } yield insert(i, h)

  lazy val genHeap: Gen[H] = gen(0)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  
  property("gen1") = forAll(genHeap) { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("On Empty, add then delete it is empty") = {
    isEmpty(deleteMin(insert(1, empty)))
  }
  
    property("Add two on Empty") = {
    findMin(insert(2, insert(1, empty))) == 1
  }

  property("Delete min then no more that min") = forAll(genHeap) { (h: H) =>
    if (isEmpty(h)) false
    else {
      val rest = deleteMin(h)
      if (isEmpty(rest)) true
      else findMin(rest) >= findMin(h)
    }
  }

  property("Min of melding is the same with that of being melded") = forAll(genHeap, genHeap) {
    (f: H, s: H) => findMin(meld(f, s)) == Math.min(findMin(f), findMin(s))
  }

  property("First min is the minist of all") = forAll(genHeap) {
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
  
  property("add and delete all") = {
    val h = ((0 until 1000).toList).foldLeft(empty)((heap, i) => insert(i, heap))
    checkAll(0, h)
  }
  def checkAll(min: Int, heap: H): Boolean = {
    if(isEmpty(heap)) true
    else {
      if(min != findMin(heap)) false
      else checkAll(min + 1, deleteMin(heap))
    }
  }

}
