import org.scalatest._
import scala.meta._

class MainSpec extends WordSpec with Matchers {

  "visitTree" should {

    "parse 'ErrorsOr[A]' Defn.Type" in {
      val errorsOr: Defn.Type = q"type ErrorsOr[A] = ValidatedNel[String, A]"

      val description = "type alias ErrorsOr of A equal to ValidatedNel of String and A"
      val summary = "alias ErrorsOr A"

      Main.visitTree(errorsOr) shouldEqual description
    }

    "parse 'a' Defn.Val" in {
      val a: Defn.Val = q"val a = 5"

      val description = "val a with value 5"
      val summary = "val a"

      Main.visitTree(a) shouldEqual description
    }

    "parse 'Monad' Defn.Trait" in {
      val monad: Defn.Trait =
        q"""trait Monad[F[_]] extends Applicative[F] {
              def one = 1
              def two = 2
            }
         """ // no stripMargin on this quasi-quote weirdly? ~mbo

      val description = "trait Monad with higher type F extending applicative of F with two declarations"
      val summary = "trait Monad higher F"

      Main.visitTree(monad) shouldEqual description
    }

    "parse 'Person' Defn.Class" in {
      val person: Defn.Class = q"case class Person(name: String, address: Address, age: Int)"

      val description = "case class Person containing name String, address Address and age Int"
      val summaryPreferSymbols = "case class Person with name, address and age"
      val summaryPreferTypes = "case class Person with String, Address and Int"

      Main.visitTree(person) shouldEqual description
    }
  }
}