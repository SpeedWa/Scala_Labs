import Compositions.{testCompose, testForComprehension, testMapFlatMap}
import RecursiveData.{CustomTree, getListHead, testListIntEmpty}
import RecursiveFunctions.{testAppend, testFlatMap, testMap, testReverse}

import scala.annotation.tailrec

object Output {
  def main(args: Array[String])= {
    println("RecursiveData: ")
    val resNil = testListIntEmpty(Nil[Int]())
    val resCons = testListIntEmpty(Cons[Int](1, Nil[Int]()))

    println("Пустой список checkListIntEmpty: " + resNil)
    println("Непустой список checkListIntEmpty: " + resCons)

    val nilHead = getListHead(Nil[Int]())
    val listHead = getListHead(Cons[Int](5, Nil[Int]()))

    println("Пустой список getListIntHead: " + nilHead)
    println("Непустой список getListIntHead: " + listHead)

    val leftChild = CustomTree(2, CustomTree(4))
    val rightChild = CustomTree(3)
    val head = CustomTree(1, leftChild, rightChild)

    println("Дерево: ")
    println("  " + head.leaf)
    println(" " + head.leftNode.leaf + " " + head.rightNode.leaf)
    println(head.leftNode.leftNode.leaf)

    val currList = Cons(1, Cons(2, Cons(3, Nil())))

    println("")
    println("RecursiveFunc: ")
    print("Reversed from ")
    printList(currList)
    printList(testReverse(currList))

    print("Map (f = a * a) from ")
    printList(currList)
    printList(testMap(currList, (a: Int) => a * a))

    val appList = Cons(4, Cons(5, Cons(6, Nil())))
    print("Append ")
    printList(currList)
    print("and ")
    printList(appList)
    printList(testAppend(currList, appList))

    print("FlatMap(f = [a * a, a * a * a]) from ")
    printList(currList)
    printList(testFlatMap(currList, (a: Int) => Cons(a * a, Cons(a * a * a, Nil()))))

    println("")
    println("Compositions: ")
    print("testCompose for a = a * 1, b = b * 2, c = c * 3. Use number 2: ")
    println(testCompose((a: Int) => a * 1)((b: Int) => b * 2)((c: Int) => c * 3)(2))
    print("testMapFlatMap (same result): ")
    println(
      testMapFlatMap((a: Int) => Some(a * 1))
      ((b: Int) => Some(b * 2))
      ((c: Int) => c * 3)
      (Some(2))
    )
    print("testForComprehension (same result): ")
    println(
      testForComprehension((a: Int) => Some(a * 1))
      ((b: Int) => Some(b * 2))
      ((c: Int) => c * 3)
      (Some(2))
    )
  }

  @tailrec
  def printList[A](list: List[A]): Unit = list match {
    case Cons(head, tail) => {
      print(head + " ")
      printList(tail)
    }
    case Nil() => println("")
  }
}