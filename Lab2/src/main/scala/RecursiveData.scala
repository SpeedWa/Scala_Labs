

sealed trait List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
case class Nil[A]() extends List[A]

/** Напишите свои решения в виде функций. */
object RecursiveData {

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.

  def isListEmpty(list: List[Int]): Boolean = list match {
    case Nil() => true
    case _ => false
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = isListEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.

  def getListHead(list: List[Int]): Int = list match {
    case Cons(head, _) => head
    case _ => -1
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = getListHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?


  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */

  case class CustomTree[A](leaf: A, leftNode: CustomTree[A] = null, rightNode: CustomTree[A] = null)
}
