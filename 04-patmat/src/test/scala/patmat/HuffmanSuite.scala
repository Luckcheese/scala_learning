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
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("compute how many times each letter appears on the text") {
    val string: List[Char] = string2Chars("aaaBBBBB")
    val counted: List[(Char, Int)] = times(string)

    if (counted.head._1 == 'a') {
      assert(counted.head === ('a', 3))
      assert(counted.tail.head === ('B', 5))
      assert(counted.tail.tail.isEmpty)
    }
    else {
      assert(counted.head === ('B', 5))
      assert(counted.tail.head === ('a', 3))
      assert(counted.tail.tail.isEmpty)
    }
  }

  test("compute how many times each letter appears on the text - 2") {
    val string: List[Char] = string2Chars("BaBBaBaB")
    val counted: List[(Char, Int)] = times(string)

    if (counted.head._1 == 'a') {
      assert(counted.head === ('a', 3))
      assert(counted.tail.head === ('B', 5))
      assert(counted.tail.tail.isEmpty)
    }
    else {
      assert(counted.head === ('B', 5))
      assert(counted.tail.head === ('a', 3))
      assert(counted.tail.tail.isEmpty)
    }
  }

  test("if tree was created correctly") {
    assert(createCodeTree(string2Chars("bacbcdc")) === Fork(Leaf('c', 3), Fork(Leaf('b', 2), Fork(Leaf('a', 1), Leaf('d', 1), List('a', 'd'), 2), List('b', 'a', 'd'), 4), List('c', 'b', 'a', 'd'), 7))
  }

  test("decode") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === List('a', 'b'))
      assert(decode(t1, List(0, 0, 1, 1, 0, 1, 1, 0)) === List('a', 'a', 'b', 'b', 'a', 'b', 'b', 'a'))

      assert(decode(t2, List(0, 0, 1, 1, 0, 1, 1, 0, 1)) === List('a', 'd', 'd', 'b', 'd', 'b'))
    }
  }

  test("encode") {
    new TestTrees {
      assert(encode(t1)(List('a', 'b')) === List(0, 1))
      assert(encode(t1)(List('a', 'a', 'b', 'b', 'a', 'b', 'b', 'a')) === List(0, 0, 1, 1, 0, 1, 1, 0))

      assert(encode(t2)(List('a', 'd', 'd', 'b', 'd', 'b')) === List(0, 0, 1, 1, 0, 1, 1, 0, 1))
    }
  }

  test("merge codetables") {
    val left = ('a', List(1, 0)) :: Nil
    val right = ('b', List(0)) :: Nil
    val result = ('a', List(0, 1, 0)) :: ('b', List(1, 0)) :: Nil
    assert(mergeCodeTables(left, right) === result)

    val result2 = ('a', List(0, 0, 1, 0)) :: ('b', List(0, 1, 0)) :: ('a', List(1, 0, 1, 0)) :: ('b', List(1, 1, 0)) :: Nil
    assert(mergeCodeTables(result, result) === result2)
  }
}
