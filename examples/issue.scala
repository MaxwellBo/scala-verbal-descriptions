object Main {

  type ErrorsOr[A] = ValidatedNel[String, A]
  // type alias ErrorsOr type A equal to ValidatedNel of String and type A
  // alias ErrorsOr A


  def foo[A: Wibble](s: String, b: Wobble[A]): Int = {
    val a = 5
    // val a with value 5 
    // val a
    a
  }
}

trait Monad[F[_]] extends Applicative[F] {
    // five methods defined here...
}
// trait Monad with higher type F extending applicative of F with five declarations
// trait Monad higher F

case class Person(name: String, address: Address, age: Int)
// case class Person containing name String, address Address and age Int
// --summary --prefer=symbols: case class Person with name, address and age
// --summary --prefer=types:   case class Person with String, Address and Int
