

import scala.annotation.tailrec

/** Реализуйте функции для решения следующих задач.
  * Примечание: Попытайтесь сделать все функции с хвостовой рекурсией, используйте аннотацию для подстверждения.
  * рекурсия будет хвостовой если:
  *   1. рекурсия реализуется в одном направлении
  *   2. вызов рекурсивной функции будет последней операцией перед возвратом
  */
object RecursiveFunctions {

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(rem: List[A], agg: Int): Int = rem match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }

    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   */

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(last: List[A], curr: List[A]): List[A] = curr match {
      case Cons(head, tail) => loop(Cons(head, last), tail)
      case Nil()         => last
    }

    loop(Nil[A](), list)
  }

  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse(list)

  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */

  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(last: List[B], curr: List[A]): List[B] = curr match {
      case Cons(head, tail) => loop(Cons(f(head), last), tail)
      case Nil()         => last
    }

    reverse(loop(Nil(), list))
  }

  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)
  
  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */

  def append[A](l: List[A], r: List[A]): List[A] = {
    @tailrec
    def loop(list: List[A], appList: List[A]): List[A] = list match {
      case Cons(head, tail) => loop(tail, Cons(head, appList))
      case Nil()         => appList
    }

    loop(reverse(l), r)
  }

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = append(l, r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   * 
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в 
   *    списке. Поэтому вы создаете List[List[B]]. 
   */

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
    @tailrec
    def loop(last: List[B], curr: List[A]): List[B] = curr match {
      case Cons(head, tail) => loop(append(f(head), last), tail)
      case Nil()         => last
    }

    loop(Nil(), reverse(list))
  }

  // используйте функцию из пункта  (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[B] = flatMap(list)(f)

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */
}
