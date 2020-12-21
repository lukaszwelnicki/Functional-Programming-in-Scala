package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(const(this.empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("should return min of two elements added to empty heap") = forAll { (a: Int, b: Int) =>
    val min = Math.min(a, b)
    findMin(insert(a, insert(b, empty))) == min
  }

  property("should empty a single-element heap by removing minimum element") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("elements in a heap should be ordered") = forAll { h: H =>
    def isSorted(h: H): Boolean = {
      @tailrec
      def isSortedLoop(lastSeen: A, h: H): Boolean = {
        if (isEmpty(h)) true
        else lastSeen <= findMin(h) && isSortedLoop(findMin(h), deleteMin(h))
      }

      if (isEmpty(h)) true else isSortedLoop(findMin(h), h)
    }

    isSorted(h)
  }

  property("minimum of the melding of two heaps is the minimum of one of them") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("meld should be associative") = forAll { (h1: H, h2: H, h3: H) =>
    findMin(meld(meld(h1, h2), h3)) == findMin(meld(h1, meld(h2, h3)))
  }

  property("meld should produce equal heaps even after swapping elements") = forAll { (h1: H, h2: H) =>
    @tailrec
    def heapsEqual(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else if (isEmpty(h1) || isEmpty(h2)) false
      else {
        findMin(h1) == findMin(h2) && heapsEqual(deleteMin(h1), deleteMin(h2))
      }
    }

    heapsEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
