
/* 
 * 
 a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 * 
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
 * 
 * b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
 *      - cat, mammal, meat
 *      - parrot, bird, vegetables
 *      - goldfish, fish, plants
 * 
 *    Синтаксис: object MyClass {
 *              // статические поля и методы
 *            }
 * 
 * c) Добавьте следующие метод в Animals:
 *      def eats(food: String): Boolean
 *    
 *     который проверяет ест ли животное определенную пищу
 * 
 * d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
 *    Вам все еще нужно поле `species`?
 * 
 * e) Добавьте следующие функции в объект-компаньон Animal:
 *      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
 *      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
 * 
 * f) Создайте трейт Food со следующими классами-образцами:
 *      - Meat
 *      - Vegetables
 *      - Plants
 *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
 *      def apply(food: String): Option[Food]
 */

trait Animal {
  val name: String
  val food: Food

  def eats(food: Food): Boolean = {
    this.food == food
  }
}

object Animal {
  val cat = new Mammal("cat", new Meat())
  val parrot = new Bird("parrot", new Vegetables())
  val goldfish = new Fish("goldfish", new Plants())

  def knownAnimal(name: String): Boolean =
    name == cat.name || name == parrot.name || name == goldfish.name

  def apply(name: String): Option[Animal] = name match {
    case "cat" => Some(cat)
    case "parrot" => Some(parrot)
    case "goldfish" => Some(goldfish)
    case _ => None()
  }
}

case class Mammal(val name: String, val food: Food) extends Animal
case class Bird(val name: String, val food: Food) extends Animal
case class Fish(val name: String, val food: Food) extends Animal

object Food {
  val meat: Meat = Meat()
  val vegetables: Vegetables = Vegetables()
  val plants: Plants = Plants()

  def apply(food: String): Option[Food] = food match {
    case "meat" => Some(meat)
    case "vegetables" => Some(vegetables)
    case "plants" => Some(plants)
    case _ => None()
  }
}

trait Food

case class Meat() extends Food
case class Vegetables() extends Food
case class Plants() extends Food

sealed trait Option[A] {
 
  def isEmpty: Boolean
}
case class Some[A](a: A) extends Option[A] {
  val isEmpty = false
}
case class None[A]()     extends Option[A] {
  val isEmpty = true
}
