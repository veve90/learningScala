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
    //hoffmaniscool
    val t3 =
    Fork(
    Fork(
    Fork(
      Fork(
        Fork(
          Fork(
            Fork(
              Fork(Fork(Leaf('f',2), Leaf('o',3), List('f','o'), 5), Leaf('h',1), List('f','o','h'), 6)
                , Leaf('m',1), List('f','o','h','m'), 7
            ),
            Leaf('a',1), List('f','o','h','m','a'), 8
          ),
          Leaf('n',1), List('f','o','h','m','a','n'), 9
        ),
          Leaf('i',1), List('f','o','h','m','a','n','i'), 10
        ),
        Leaf('s',1), List('f','o','h','m','a','n','i','s'), 11
      ),
      Leaf('c',1), List('f','o','h','m','a','n','i','s','c'), 12
    ),
      Leaf('l',1), List('f','o','h','m','a','n','i','s','c','l'), 13
    )
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


  test("createCodeTree personal test") {
    new TestTrees {
      assert(chars(createCodeTree("ab".toList)) === List('a','b'))
    }
  }

  test("createCodeTree personal test2") {
    new TestTrees {
      assert(chars(createCodeTree("abcd".toList)) === List('b', 'd', 'a', 'c'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a very short text should be identity - big tree") {
    new TestTrees {
      assert(decode(t3, encode(t3)("fh".toList)) === "fh".toList)
    }
  }




}
