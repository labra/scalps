package samples

object Origami extends App {

  def id [a] (x :a ) : a = x
    
  case class Fix[F[_,_],a] (out: F[a,Fix[F,a]]) 
  
  trait BiFunctor[F[_,_]] {
    def bimap[a,b,c,d]:(a => b) => (c => d) => F[a,c] => F[b,d]
    def fmap2[a,c,d] : (c => d) => F[a,c] => F[a,d] =
      bimap(id)
  }
  
  def map[a,b,F[_,_]] (f: a => b) (t : Fix[F,a]) (implicit ft: BiFunctor[F]) : Fix[F,b] =
    Fix[F,b] (ft.bimap(f) (map[a,b,F](f)) (t.out))
    
  def cata[a,r,F[_,_]](f:F[a,r] => r) (t:Fix[F,a]) (implicit ft: BiFunctor[F]):r =
    f (ft.fmap2 (cata[a,r,F](f)) (t.out))

  def build[a,F[_,_]] (f : { def apply[b] : (F[a,b] => b) => b}) = 
    f.apply(Fix[F,a])


  trait ListF[a,r]
  case class Nil[a,r]() extends ListF[a,r]
  case class Cons[a,r] (x:a,xs:r) extends ListF[a,r]

  implicit object biList extends BiFunctor[ListF] {
    def bimap[a,b,c,d] = f => g => {
      case Nil() => Nil()
      case Cons(x,xs) => Cons (f (x),g (xs))
    }
  }
  
  type List[a] = Fix[ListF,a]
  
  def nil [a] : List[a] = Fix(Nil ())
  def cons[a] = (x:a) => (xs:List[a]) => Fix (Cons(x,xs))

//  def nil[a] : List[a] = In [ListF,a] (Nil ())
//  def cons[a] = (x:a) => (xs:List[a]) => In [ListF,a] (Cons (x,xs))
  
 def sumList = cata [Int,Int,ListF] {
    case Nil () => 0
    case Cons (x,n) => x + n
  } _

 def prodList = cata [Int,Int,ListF] {
    case Nil () => 1
    case Cons (x,n) => x * n
  } _
  
}