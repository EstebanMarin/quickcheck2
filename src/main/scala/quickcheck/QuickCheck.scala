package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =
    oneOf(
      const(empty),
      for
        v <- arbitrary[Int]
        h <- oneOf(const(empty), genHeap)
      yield insert(v, h)
    )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of two elements") = forAll { (x1: Int, x2: Int) =>
    val heap = insert(x2, insert(x1, empty))
    val min = if x1 <= x2 then x1 else x2
    findMin(heap) == min
  }

  property("delete min one") = forAll({ (x: Int) =>
    val heap1 = insert(x, empty)
    val heap0 = deleteMin(heap1)
    isEmpty(heap0)
  })

  property("insert minimum and then finding it should return the same min") =
    forAll(genHeap.suchThat((x: H) => !isEmpty(x))) { (h: H) =>
      val min = findMin(h)
      val insertHeap = insert(min, h)
      findMin(insertHeap) == min
    }

  property("delete all produce sorted list") =
    def check(heap: H): Boolean =
      // if the heap is empty, or if it has just one element, we have
      // successfully finished our checks
      if isEmpty(heap) || isEmpty(deleteMin(heap)) then true
      else
        // find the minimal element
        val x1: Int = findMin(heap)
        // delete the minimal element of `heap`
        val heap2: H = deleteMin(heap)
        // find the minimal element in `heap2`
        val x2: Int = findMin(heap2)
        // check that the deleted element is less than or equal to the
        // minimal element of the remaining heap, and that the remaining
        // heap verifies the same property (by recursively calling `check`)
        val checked: Boolean =
          if x1 <= x2 then check(heap2) else false
        checked
    forAll { check(_) }

  property("melding check") = forAll { (x: Int, y: Int) =>
    val maxi: H = insert(x.max(y), insert(x.max(y), empty))
    val minim: H = insert(x.min(y), insert(x.min(y), empty))
    val meldedHeap: H = meld(maxi, minim)
    val deleteTwoMinAndFindMin: Boolean =
      findMin(deleteMin(deleteMin(meldedHeap))) == x.max(y)
    val insertMinAndFindMin: Boolean =
      findMin(insert(x.min(y), meldedHeap)) == x.min(y)
    deleteTwoMinAndFindMin && insertMinAndFindMin
  }

  property("melding more check") = forAll { (heap1: H, heap2: H) =>
    val melded = meld(heap1, heap2)
    def check(
        melded: H,
        hp1: H,
        hp2: H
    ): Boolean =
      (melded, hp1, hp2) match
        case (Nil, Nil, Nil) => true
        case (m, hp1, hp2) =>
          if !isEmpty(hp1) && findMin(m) == findMin(hp1)
          then check(deleteMin(m), deleteMin(hp1), hp2)
          else if !isEmpty(hp1) && findMin(m) == findMin(hp2) then
            check(deleteMin(m), hp1, deleteMin(hp2))
          else false
    check(melded, heap1, heap2)
  }
