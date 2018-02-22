import scala.meta._
import java.io.File
import scopt.OptionParser
import scala.io.Source

object Location {
  val START_OF_FILE = Location(0, 0)
}

final case class Location(
  line: Int,
  character: Int
) {
  def lineStart = this.copy(character = 0)
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
  mode: Mode.Value = Mode.Summarise,
  prefer: Preference.Value = Preference.Types,
  target: Either[Location, LocationRange] = Left(Location(0, 0)),
  layers: Int = 3
)

object Main extends App {
  // For development
  go()(Config().copy(input = new File("./src/main/scala/Main.scala")))

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
    val file = Source.fromFile(cfg.input)

    println(interpret(file.mkString))
  }

  def interpret(xs: String)(implicit cfg: Config): String = {
    "interpretation"
  }
}

