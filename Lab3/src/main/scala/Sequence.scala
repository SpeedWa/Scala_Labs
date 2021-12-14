
import scala.annotation.tailrec

/** Напишите свои решения в тестовых функциях.
  * 
  * Seq(1, 2) match {
  *   case head +: tail => ???
  *   case Nil          => ???
  *   case s            => ???
  * }
  * 
  * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
  */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *    
   */
  def testLastElement[A](seq: Seq[A]): Option[A] = {
    @tailrec
    def loop(last: Seq[A]): Option[A] = last match {
      case head +: Seq() => Option.apply(head)
      case _ +: tail => loop(tail)
      case Seq() => None
    }

    loop(seq)
  }

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *    
   */
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = Seq((a.head, b.head), (a(1), b(1)))

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *    
   */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop(last: Seq[A], curr: Seq[A]): Seq[A] = curr match {
      case head +: Seq() => Seq(head) ++ last
      case head +: tail => loop(Seq(head) ++ last, tail)
      case Seq() => Seq()
    }

    seq == loop(Seq(), seq)
  }

  /* d) Проверьте, является ли Seq палиндромом
   *    
   */
  def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop(last: Seq[A], curr: Seq[A]): Seq[A] = curr match {
      case head +: Seq() => Seq(head) ++ last
      case head +: tail => loop(Seq(head) ++ last, tail)
      case Seq() => Seq()
    }

    seq == loop(Seq(), seq)
  }

  /* e) Реализуйте flatMap используя foldLeft.
   *    
   */
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((a, b) => a ++ f(b))
}
