package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Prop.BooleanOperators

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def insertAll(values: List[Int], h: H): H = values match {
    case Nil => h
    case v :: vs => insertAll(vs, insert(v, h))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of 2 elements should be found with findMin") = forAll { (h: H, x: Int, y: Int) =>
    (isEmpty(h) & x < y) ==> {
      val h2 = insert(y, insert(x, h))
      findMin(h2) == x
    }
  }

  property("inserting and deleting one element on empty heap, produces empty heap") = forAll { (h: H, x: Int) =>
    (isEmpty(h)) ==> {
      val h2 = insert(x, h)
      val h3 = deleteMin(h2)
      isEmpty(h3)
    }
  }

  property("heap is ordered") = forAll { (h: H) =>
    def isOrdered(h: H): Boolean =
      if (isEmpty(h))
        true
      else {
        val m = findMin(h)
        val rest = deleteMin(h)
        isEmpty(rest) || (m <= findMin(rest) & isOrdered(rest))
      }

    isOrdered(h)
  }

  property("by melding two heap we get on heap whose minimum element is one of the 2 heaps' minimum elements") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) & (!isEmpty(h2))) ==> {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val h3 = meld(h1, h2)
      val m3 = findMin(h3)
      m3 == m1 | m3 == m2
    }
  }

  property("if we know the 2 minimum elements and exclude the first one, the minimum remain the second one") = forAll { (h: H, x: Int, y: Int) =>
    (isEmpty(h) & x < y) ==> {
      val h2 = insert(y, insert(x, h))
      val h3 = deleteMin(h2)
      findMin(h3) == y
    }
  }

  property("just a generation experiment") = forAll { (h: H, x: Int, y: Int, values: List[Int]) =>
    (isEmpty(h) & x < y & values.forall(v => y < v)) ==> {

      val h2 = insertAll(values, insert(x, insert(y, h)))
      val h3 = deleteMin(h2)
      findMin(h3) == y
    }
  }

  property("if we have two equal minimums and remove one, the other is again the minimum") = forAll { (h: H, x: Int) =>
    (isEmpty(h)) ==> {

      val h2 = insert(x, insert(x, h))
      val h3 = deleteMin(h2)
      findMin(h3) == x
    }
  }

  property("if we move the minimum element from one heap to another one and merge the two heaps, the mininum should be the same") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) & !isEmpty(h2)) ==> {

      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val h3 = meld(deleteMin(h1), insert(m1, h2))
      val h4 = meld(deleteMin(h2), insert(m2, h1))
      findMin(h3) == findMin(h4)

    }
  }

}
