import scala.meta._
import java.io.File
import scopt.OptionParser
import scala.io.{Source => FileSource}

object Location {
  val START_OF_FILE = Location(0, 0)
}

final case class Location(
  line: Int,
  character: Int
) {
  def lineStart: Location = this.copy(character = 0)
}

final class LocationRange(
  from: Location,
  to: Location
)

object Mode extends Enumeration {
   val Summarise  = Value("summarise")
   val Describe   = Value("describe")
   val Breadcrumb = Value("breadcrumb")
} 

object Preference extends Enumeration {
   val Symbols = Value("symbols")
   val Types   = Value("types")
} 

final case class Config(
  input: File = new File("."),
  mode: Mode.Value = Mode.Describe,
  prefer: Preference.Value = Preference.Types,
  target: Either[Location, LocationRange] = Left(Location(0, 0)),
  layers: Int = 3
)

object Main extends App {

  implicit class TreeOps(self: Tree) {
    def visitTree = visit(self)
  }

  // For development
  go()(Config().copy(input = new File("./examples/issue.scala")))

  def parseArgs(): Option[Config] = {
    val NAME = "scala-verbal-descriptions"

    val parser = new OptionParser[Config](NAME) {
      opt[File]("input")
        .required()
        .valueName("<file>")
        .action( (x, c) => c.copy(input = x) )
        .text("input is a required file property")
    }

    parser.parse(args, Config())
  }

  def go()(implicit cfg: Config): Unit = {
    val file = FileSource.fromFile(cfg.input)

    println(interpret(file.mkString))
  }

  def interpret(xs: String)(implicit cfg: Config): String = {
    cfg.mode match {
      case Mode.Describe => describe(xs)
      case Mode.Summarise => summarise(xs)
      case Mode.Breadcrumb => breadcrumb(xs)
    }
  }

  def visitList(xs: List[Tree]): String = {
    xs.map(visit).mkString(", ")
  }

  def visit(tree: Tree): String = {
    tree match {
      case _import: Import =>
        "An import"
      case typeName: Type.Name =>
        s"typeName.value"
      case typeApply: Type.Apply =>
        visitList(typeApply.args)
      case defnObject: Defn.Object =>
        f"Object definition ${visit(defnObject.name)} with children ${visitList(defnObject.children)}"
      case termName: Term.Name =>
        termName.value

      case x => f"[HALT] ${x.productPrefix}"
    }
  }

  def recursiveExplore(stats: List[Tree])(level: Int): Unit = {
    println(stats.map(visit).mkString("\n"))
  }

  def describe(xs: String)(implicit cfg: Config): String = {
    val tree = xs.parse[Source].get.stats

    recursiveExplore(tree)(0)

    ""
  }

  def summarise(xs: String)(implicit cfg: Config): String = {
    ""
  }

  def breadcrumb(xs: String)(implicit cfg: Config): String = {
    ""
  }
}

