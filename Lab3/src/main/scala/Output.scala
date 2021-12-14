import Adts.{testDouble, testGetNth, testGoodOldJava, testIsEven, testSafeDivide}
import Maps.{User, testGroupUsers, testNumberFrodos, testUnderaged}
import Sequence.{testFlatMap, testForAll, testLastElement, testPalindrom, testZip}
import Strings.{testComputation, testInterpolations, testTakeTwo, testUppercase}

object Output{
  def main(args: Array[String])= {
    println("Adts.scala")
    println("Элемент с индексом 3 от List(1, 2, 3, 4, 5, 6): " + testGetNth(List(1, 2, 3, 4, 5, 6), 3));
    println("3 * 2: " + testDouble(Some(3)))
    println("4 (isEven): " + testIsEven(4))
    println("5 (isEven): " + testIsEven(5))
    println("6 / 2 (safeDivide): " + testSafeDivide(6, 2))
    println("6 / 0 (safeDivide): " + testSafeDivide(6, 0))
    println("str.toInt: " + testGoodOldJava((str: String) => str.toInt, "123"))
    println("(5 / 0) + str.toInt: " + testGoodOldJava((str: String) => (5 / 0) + str.toInt, "123"))
    println("")

    println("Maps.scala")
    println("Средний возраст: " + testGroupUsers(Seq(User("Петр", 5), User("Петр", 15), User("Иван", 4), User("Иван", 10))))
    println(
      "Количество пользователей с 'Adam' в имени: " +
        testNumberFrodos(Map("a" -> User("Adam Ivanov", 10), "b" -> User("Adam Petrov", 10), "c" -> User("Ivan Ivanov", 10)))
    )
    println(
      "Пользователи старше 35: " +
        testUnderaged(Map("a" -> User("Adam Ivanov", 20), "b" -> User("Adam Petrov", 40), "c" -> User("Ivan Ivanov", 30)))
    )
    println("");

    println("Sequence.scala")
    println("Последний элемент из Seq(1, 2, 3): " + testLastElement(Seq(1, 2, 3)))
    println("Соединение (1 и 3, 2 и 4): " + testZip(Seq(1, 2), Seq(3, 4)))
    println("Четные ли все элементы в Seq(1, 4, 8)" + testForAll(Seq(1, 4, 8))(a => a % 2 == 0))
    println("Четные ли все элементы в Seq(2, 4, 8)" + testForAll(Seq(2, 4, 8))(a => a % 2 == 0))
    println("Является ли Seq(1, 2, 1) палиндромом: " + testPalindrom(Seq(1, 2, 1)))
    println("Является ли Seq(1, 2, 3) палиндромом: " + testPalindrom(Seq(1, 2, 3)))
    println("Flatmap для Seq(1, 2, 3) (Квадрат числа, куб числа): " + testFlatMap(Seq(1, 2, 3))(a => Seq(a * a, a * a * a)))
    println("");

    println("Strings.scala")
    println("Привести 'abc' к верхнему регистру: " + testUppercase("abc"))
    println(testInterpolations("Ivan", 10))
    println(testComputation(10, 5))
    println("Взять первые два символа из 'abc': " + testTakeTwo("abc"))
    println("")
  }
}