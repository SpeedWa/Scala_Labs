import scala.math.BigInt.probablePrime
import scala.util.Random

object tasks {

  def task7(word: String): Unit = {
    println(word.charAt(0))
    println(word.charAt(word.length - 1))
  }

  def task8(word: String): Unit = {
    println(word.take(2))
    println(word.drop(2))
    println(word.takeRight(1))
    println(word.dropRight(1))
    println(word.substring(5))
  }

  def task9(num: Int): Int = {
    math.signum(num)
  }

  def task11(): Unit = {
    for (i <- 10 to 0 by -1)
      print(i + " ")
    println("")
  }

  def task12(n: Int): Unit = {
    if (n > 0)
      for (i <- n to 0 by -1)
        print(i + " ")
    else
      print("Число меньше или равно 0 ")
    println("")
  }

  def task13(s: String): Unit = {
    for (i <- 0 to s.length - 1)
      println(s.charAt(i).toInt)
  }

  def task14(s: String): Long = {
    s.foldLeft(1L)((m, n) => m * n)
  }

  def task16(s: String): Long = {
    val n = s.head.toInt

    if (s.length == 1)
      return n

    n * task16(s.takeRight(s.length - 1))
  }

  def task17(x: Int, n: Int): Int= {
    if (n == 0) {
      return 1
    }

    if (n > 0) {
      if (n % 2 == 0) {
        return task17(x, n / 2) * task17(x, n / 2)
      } else {
        return x * task17(x, n - 1)
      }
    }

    1 / task17(x, -n)
  }

  def task18(m: Int, n: Int): Int = {

    var sum = 0

    for (i <- m to n) {
      val map = scala.collection.mutable.HashMap.empty[String, Boolean]
      val str = i.toString
      val list = str.split("")
      var status = true

      for (ch <- list) {
        if (map.contains(ch)) {
          status = false
        }

        map += ((ch, true))
      }

      if (status) {
        sum += i
      }
    }

    sum
  }

  def task19(list: List[Any]): List[Int] = {
    var resList: List[Int] = List()

    for (i <- list) {
      if (i.isInstanceOf[Int]) {
        resList :+= i.toString.toInt
      } else {
        resList :++= task19(i.asInstanceOf[List[Any]])
      }
    }

    resList
  }

  def maxDel(num: Int): Int = {
    var del = (num / 2).toInt

    while (del * del > 1) {
      if (num % del == 0) {
        return del
      }

      del -= 1
    }

    0
  }

  def task20(num: Int): Int = {
    var sum = 0
    val del = maxDel(num)
    val list = del.toString.split("")

    for (i <- list) {
      sum += i.toInt
    }

    sum
  }

  def task21(list: List[Any], k: Int): List[Any] = {
    var kList: List[Any] = List()

    for (i <- list) {
      for (j <- 1 to k) {
        kList :+= i
      }
    }

    kList
  }

  def nod(a: Int, b: Int): Int = {
    if (b == 0) a else nod(b, a % b);
  }

  def task24(a: Int, b: Int): Int = a * b / nod(a, b)

  def task25(list: List[Any], k: Int): List[Any] = {
    list.grouped(k).flatMap(_.take(k - 1)).toList
  }

  def task26(n: Int, k: Int): Int = {
    if (k == n || k == 0)
      return 1
    if (k != 1)
      task26(n-1, k) + task26(n-1, k-1)
    else
      n
  }

  def task27(list: List[Any], k: Int): List[Any] = {
    var res = list
    if (k > 0) {
      for (i <- 1 to k) {
        res = res.takeRight(list.length - 1) ++ res.take(1)
      }
    }
    else {
      for (i <- 1 to -k) {
        res = res.takeRight(1) ++ res.take(list.length - 1)
      }
    }
    res
  }

  def task28(n: Int): Int = {
    var list: List[Int] = List()

    for (i <- 1 to n) {
      var sum = 0
      for (j <- 1 until i - 1) {
        if (i % j == 0) {
          sum += j
        }
      }

      if (sum == i) {
        list :+= i
      }
    }

    list.max
  }

  def task29(list: List[Any]): (List[Any], List[Any]) = {
    var even: List[Any] = List()
    var odd: List[Any] = List()
    var k = 1

    for (i <- list) {
      if (k % 2 == 0) {
        even :+= i
      } else {
        odd :+= i
      }
      k += 1
    }

    (even, odd)
  }

  def task30(n: Int): Int = {
    for (i <- n to 1 by -1) {
      val s = i.toString.split("").map(_.toInt).sum
      var p = s

      do {
        p *= s
      } while (p < i)

      if (p == i) {
        return i
      }
    }

    1
  }

  def task31(list: List[(Int, String)]): (List[Int], List[String]) = {
    var num: List[Int] = List()
    var str: List[String] = List()

    for (i <- list) {
      num :+= i._1
      str :+= i._2
    }

    (num, str)
  }

  def main(args: Array[String]): Unit = {

    println("2) " + ("crazy" * 3))

    println("3) " + (10 max 2))

    println("4) " + BigInt(2).pow(1024))

    println("5) " + probablePrime(100, Random))

    println("6) " + probablePrime(100,Random).toString(36))

    println("7) ")
    task7("texting")

    println("8) ")
    task8("abcdefg")

    println("9) " + task9(10))

    println("10) " + {})

    println("11) ")
    task11()

    println("12) ")
    task12(10)

    println("13) ")
    task13("Hello")

    println("14) " + task14("Hello"))

    println("16) " + task16("Hello"))

    println("17) " + task17(5, 3))

    println("18) " + task18(10, 50))

    println("19) " + task19(List(1, 2, 3, List(4, 5, 6, List(7, 8, 9)))))

    println("20) " + task20(35))

    println("21) " + task21(List(1, "2", "4", List(1, 2, 3)), 2))

    println("24) " + task24(24, 6))

    println("25) " + task25(List(1, 2, 3, 4, 5, 6, 7), 3))

    println("26) " + task26(15, 4))

    println("27) " + task27(List(1, 2, 3, 4, 5, 6, 7), 3))

    println("28) " + task28(60))

    println("29) " + task29(List(1, 2, 3, 4, 5, 6, 7, 8)))

    println("30) " + task30(513))

    println("31) " + task31(List((1, "abc"), (2, "def"), (3, "ghi")) ))
  }
}
