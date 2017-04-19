package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
     a <- arbitrary[A]
     b <- oneOf(const(empty), genHeap)
  } yield insert(a, b)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("minimum after one insert") = forAll { a: Int =>
  val h = insert(a, empty)
  findMin(h) == a
  }
  
  property("minimum after 2 inserts") = forAll { a: (Int, Int) =>
  val h = insert(a._1, empty)
  val h1 = insert(a._2, h)
  findMin(h1) == List(a._1, a._2).min
  }  
  
  property("minimum after 1 insert and 1 delete") = forAll { a: Int =>
  val h = insert(a, empty)
  val h1 = deleteMin(h)
  isEmpty(h1)
  }  
  
  property("genHeap check minimum after one insert") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("genHeap successive min element should be in increasing order") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val h1 = deleteMin(h)
    val n = if (isEmpty(h1)) 0 else findMin(h1)
    (m != 0 && n != 0) ==> m <= n
  }
  
  property("genHeap successive min element should be in increasing order - recursive call") = forAll { (h: H) =>
    def rmMin(l:H, ll:List[A]): List[A] = {
      if(isEmpty(l)) ll
      else findMin(l) :: rmMin(deleteMin(l), ll)      
    }
    val elements = rmMin(h, List())
    elements == elements.sorted    
  }
  
  property("genHeap after meld the min should be min of individual heaps") = forAll { h:(H, H) =>
    val m1 = findMin(h._1)
    val m2 = findMin(h._2)
    findMin(meld(h._1, h._2)) == List(m1, m2).min
  }
  
  property("genHeap after meld the min should be min of meld after element is tranferred from h1 to h2") = forAll { h:(H, H) =>
    val m1 = findMin(h._1)
    val m2 = findMin(h._2)
    findMin(meld(h._1, h._2)) == findMin(meld(deleteMin(h._1), insert(m1, h._2)))
  }
  
  property("genHeap after meld the min should be min of meld after element is tranferred from h2 to h1") = forAll { h:(H, H) =>
    val m1 = findMin(h._1)
    val m2 = findMin(h._2)
    findMin(meld(h._1, h._2)) == findMin(meld(insert(m2, h._1), deleteMin(h._2)))
  }
  
  property("sequence of elements after meld") = forAll { h:(H, H) =>
    def rmMin(l:H, ll:List[A]): List[A] = {
      if(isEmpty(l)) ll
      else findMin(l) :: rmMin(deleteMin(l), ll)      
    }
    val mh1 = meld(h._1, h._2)
    val m1 = findMin(h._1)
    val mh2 = meld(deleteMin(h._1), insert(m1, h._2))
    val xs1 = rmMin(mh1, Nil)
    val xs2 = rmMin(mh2, Nil)
    xs1 == xs2
  }
}
