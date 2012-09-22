package samples

object ImplicitTypeClass extends App {

  trait Ord[T] {
    def compare(x: T, y: T): Boolean

  }
  def pick[T](x: T, y: T)(implicit ord: Ord[T]): T = {
    if (ord.compare(x, y)) x
    else y
  }

  def cmp[a](x: a, y: a)(implicit ord: Ord[a]): Boolean =
    ord.compare(x, y)

  implicit object ordInt extends Ord[Int] {
    def compare(x: Int, y: Int) = x <= y
  }

  class Apple(x: Int) {}

  /*object ordApple extends Ord[Apple] {
  def compare(a1:Apple, a2:Apple) : Boolean = a1.x <= a2.x 
}*/

  trait Show[T] {
    def show(x: T): String
  }

  trait Read[T] {
    def read(x: T): String
  }

  trait Coerce[A, B] {
    def coerce(x: A): B
  }

  /*val a1 = Apple(5)
val a2 = Apple(7)
val a3 = pick(a1,a2) (ordApple)
val a4 = pick(a2,a1) (ordApple)*/

  val x = 5
  val y = 7
  pick (x, y) (ordInt)
  pick (x, y) 
}