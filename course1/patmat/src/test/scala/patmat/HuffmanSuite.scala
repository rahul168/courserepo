package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val l1 = List(('a',2), ('b', 1))
    val l2 = List(('b',1),('c', 3))
    val l3 = List(('a', 2),('b', 2),('c', 3))
    val l4= List('a', 'b', 'a', 'c', 'c', 'b', 'c')
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("merge") {
    new TestTrees {
      assert(merge(l1, l2) == l3)
    }
  }
  
  test("times") {
    new TestTrees {
      assert(times(l4) == l3)
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      println("Tree: " + t1)
      println("Encode: " + encode(t1)("ab".toList))
      println("Decode: " + decode(t1, encode(t1)("ab".toList)))
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
