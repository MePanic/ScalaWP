package test

object Monad extends App {

  trait TypeCons[F[_]] {
    def fAToB[A, B](fa: F[A])(f: A => B): F[B]
  }

  def listCons: TypeCons[List] = new TypeCons[List] {
    def fAToB[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

  def optionCons: TypeCons[Option] = new TypeCons[Option] {
    def fAToB[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }

  println(listCons.fAToB(List("Hello", "students", "!"))(_.size))

  val romanMap = Map("I" -> 1, "II" -> 2, "III" -> 3)

  println(optionCons.fAToB(romanMap.get("III"))(2 * _))
  println(optionCons.fAToB(romanMap.get("IIII"))((i: Int) => 2 * i))
}