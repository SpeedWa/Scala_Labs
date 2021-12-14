import Functions.{testCircle, testRectangleCurried, testRectangleUc}
import HigherOrder.{plus, testAnonymousNTimes, testNTimes}
import PatternMatching.{Bird, Mammal, Paper, Rock, testExtractMammalWeight, testIntToString, testIsEven, testIsMaxAndMoritz, testUpdateFood, testWinsA}

object Output {
  def main(args: Array[String])= {
    println("Classes:");
    println("Parrot: : " + Animal.apply("parrot"))
    println("Cat eats meat: " + Animal.cat.eats(Meat()))
    println("Known animal cat: " + Animal.knownAnimal("cat"))
    println("Has meat: " + Food.apply("meat"))
    print("\n")
    println("Functions: ");
    println("testCircle: " + testCircle(1))
    println("testRectangleCurried: " + testRectangleCurried(5, 6))
    println("testRectangleUc: " +  testRectangleUc(5, 6))
    print("\n")
    println("HiOrder: ")
    println("testNTimes: " + testNTimes(plus, 10, 5, 2))
    println("testNTimes: " + testAnonymousNTimes(10, 5, 2))
    print("\n")
    println("Patterns: ")
    println("testIntToString: " + testIntToString(3))
    println("testIsMaxAndMoritz: " + testIsMaxAndMoritz("max"))
    println("testIsEven: " + testIsEven(2))
    println("rock scissors paper testWinsA: " + testWinsA(Paper, Rock))
    val dog = Mammal("Rex", PatternMatching.Meat, 20)
    val bird = Bird("Kesha", PatternMatching.Vegetables)
    println("testExtractMammalWeight: " + testExtractMammalWeight(dog))
    println("testUpdateFood: " + testUpdateFood(bird))
  }

}
