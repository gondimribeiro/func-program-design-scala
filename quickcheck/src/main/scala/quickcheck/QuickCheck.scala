package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable.Nil

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def getList(h: H): List[Int] = {
    if (isEmpty(h)) List()
    else findMin(h) :: getList(deleteMin(h))
  }

  def isSorted(l: List[Int]): Boolean = l match {
    case List() => true
    case List(_) => true
    case _ => l.sliding(2).forall { case List(x, y) => x <= y }
  }

  def insertList(l: List[Int]): H = {
    if (l.isEmpty) empty
    else insert(l.head, insertList(l.tail))
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val min1 = findMin(h2)

    val h2Del = deleteMin(h2)
    val min2 = findMin(h2Del)

    if (a < b) min1 == a && min2 == b
    else min1 == b && min2 == a
  }

  property("insertDelete") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)

    isEmpty(h2)
  }

  property("storage") = forAll { (a: Int, h: H) =>
    def checkStorage(h: H): Boolean ={
      if (isEmpty(h)) false
      else if (findMin(h) == a) true
      else checkStorage(deleteMin(h))
    }

    checkStorage(insert(a, h))
  }

  property("isSorted") = forAll { (h: H) => isSorted(getList(h)) }

  property("emptyMeld") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
      val emptyMeld = meld(h, empty)

      findMin(emptyMeld) == min
    }
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)

      val meldedH = meld(h1, h2)

      val globalMin = if (min1 < min2) min1 else min2

      findMin(meldedH) == globalMin
    }
  }
}
