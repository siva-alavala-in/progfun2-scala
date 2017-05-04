package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    oneOf(emptyHeap, singletonHeap, mixHeap)

  lazy val emptyHeap : Gen[H] = empty

  lazy val singletonHeap : Gen[H] = for {
    v <- arbInt.arbitrary
  } yield insert(v, empty)

  lazy val mixHeap : Gen[H] = for {
    v <- oneOf(singletonHeap, emptyHeap)
    r <- oneOf(singletonHeap, emptyHeap, genHeap)
    d <- arbBool.arbitrary
  } yield if (d) meld(v, r) else meld(r, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
    isEmpty(h1) || isEmpty(h2) || findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("sorted") = forAll { (h: H) =>
    def toSeq(h: H) : Seq[Int] = if (isEmpty(h)) Nil else findMin(h) +: toSeq(deleteMin(h))
    toSeq(h).equals(toSeq(h).sorted)
  }

  property("insert - delete = empty") = forAll { (h: H, x: Int) =>
    !isEmpty(h)|| isEmpty(deleteMin(insert(x, h)))
  }

  property("min(insert + insert) = deleteMin") = forAll { (h: H, x: Int, y: Int) =>
    !isEmpty(h)|| (findMin(insert(y, insert(x, h))) == Math.min(x, y) && findMin(insert(x, insert(y, h))) == Math.min(x, y))
  }

  property("2ins 2del") = forAll { (h: H, x: Int, y: Int) =>
    val empty = isEmpty(h)
    val nh = deleteMin(deleteMin(insert(x, insert(y, h))))
    !empty || isEmpty(nh)
  }

  property("merge") = forAll{ (h1: H, h2: H) =>
    def toSeq(h: H) : Seq[Int] = if (isEmpty(h)) Nil else findMin(h) +: toSeq(deleteMin(h))
    val h = meld(h1, h2)
    (toSeq(h1) ++ toSeq(h2)).sorted.equals(toSeq(h))
  }

  property("min") = forAll { (h: H) =>
    isEmpty(h)|| {
      findMin(h)
      deleteMin(h)
      true
    }
  }


}
