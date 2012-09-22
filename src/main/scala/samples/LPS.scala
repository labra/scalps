package samples

object LPS extends App {  

trait Ord[T] {
  def compare(a:T, b:T) : Boolean
}

def inserta[T] (x:T) (xs:List[T]) (implicit ordT: Ord[T]) : List[T] =
  xs match {
   case List() => List(x)
   case y::ys  => if (ordT.compare(x,y)) x::y::ys
                  else y::inserta (x) (ys)
  }

def ordena[T] (xs:List[T]) (implicit ordT: Ord[T]) : List[T] =
xs match {
case List() => List()
case y::ys   => inserta (y) (ordena (ys))
}

object reverseOrderInt extends Ord[Int] {
  def compare (a:Int, b:Int) : Boolean = a >= b
}

implicit object defaultOrderInt extends Ord[Int] {
  def compare (a:Int, b:Int) : Boolean = a <= b
}

def constOne(x:Int,y: => Int) = {
  x * x
}

  def run = {
    println("LPS System")

  }

  override def main(args : Array[String]) = run



}

