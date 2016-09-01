package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import quickcheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genMap: Gen[Map[A, A]] = for {
    k <- arbitrary[A]
    v <- arbitrary[A]
    m <- oneOf(const(Map.empty[A, A]), genMap)
  } yield m.updated(k, v)

  //--------------------------------------------------------------------------------
  // Create a heap tree 
  //--------------------------------------------------------------------------------
  def genNode(level: Int): Gen[H] = for {
    i <- arbitrary[A]
    //h <- oneOf(const(this.empty), const(insert(i, this.empty)), genNode(level + 1))
    h <- oneOf(const(insert(i, this.empty)), genNode(level + 1))
  } yield {
    //meld(empty, h)
    h
  }
  lazy val genHeap: Gen[H] = {
    genNode(0)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  property("gen1") = forAll(genHeap) { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("two elements to verify min of them") = forAll(genHeap){ (h: H) =>
    insert(Int.MinValue, h) == Int.MinValue
  }

  property("add then delete it") = {
    isEmpty(deleteMin(insert(1, empty)))
  }

  property("min of melding") = forAll(genHeap, genHeap) {
    (f: H, s: H) =>
      {
          val fm = findMin(f)
          val sm = findMin(s)
          findMin(meld(f, s)) == Math.min(fm, sm)
      }
  }

  property("sorted") = forAll(genHeap) {
    (h: H) =>
      if (isEmpty(h)) true
      else checkSorted(findMin(h), h)
  }
  def checkSorted(prev: Int, h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val cur = findMin(h)
      if (prev > cur) false
      else checkSorted(cur, deleteMin(h))
    }
  }

}
