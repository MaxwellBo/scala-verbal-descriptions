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

  implicit class StringOps(self: String) {
    def quote = self // TODO
  }

  implicit class TreeOps(self: Tree) {
    def visit: String = visitTree(self)
  }

  implicit class ListOps(self: List[Tree]) {
    private def formatList[A](xs: List[A]): String = {
      if (xs.length <= 1) {
        xs.mkString(", ") // empty lists are going to produce empty strings
      } else {
        xs.init.mkString(", ") + " and " + xs.last // safe
      }
    }

    private def visitList[A](xs: List[Tree]): String = {
      formatList(xs.map(_.visit))
    }

    private def visitListOptional(xs: List[Tree], formatString: String => String = identity): String = {
      if (xs.isEmpty) {
        ""
      } else {
        formatString(visitList(xs))
      }
    }

    def visit: String = visitList(self)
    def visitFormatNonEmpty(f: String => String): String = visitListOptional(self, f)
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


  def visitTree(tree: Tree): String = {
    tree match {
      case _import: Import =>{
        "An import"
      }

      case typeName: Type.Name => {
        typeName.value
      }

      case typeApply: Type.Apply => {
        typeApply.args.visitFormatNonEmpty(x => f"args $x")
      }

      case defnObject: Defn.Object => {
        val mods = defnObject.mods.visit
        val name = defnObject.name.visit
        val templ = defnObject.templ.visit

        f"$mods object $name has $templ"
      }

      case termName: Term.Name => {
        termName.value
      }

      case template: Template => {
        val early = template.early.visit
        val inits = template.inits.visit
        val self = template.self.visit
        val stats = template.stats.visit

        s"$early $inits $self $stats"
      }

      case defnTrait: Defn.Trait => {
        val name = defnTrait.name.visit
        val tparams = defnTrait.tparams.visitFormatNonEmpty(x => f"has type params $x")
        val mods = defnTrait.mods.visit
        val ctor = defnTrait.ctor.visit
        val template = defnTrait.templ.visit

        f"$mods trait $name $tparams $template $ctor"
      }

      case defnType: Defn.Type => {
        val name = defnType.name.visit
        val body = defnType.body.visit
        val tparams = defnType.tparams.visit
        val mods = defnType.mods.visit

        f"$mods type alias $name type $tparams equal to $body"
      }

      case typeParam: Type.Param => {
        val name = typeParam.name.visit
        val mods = typeParam.mods.visit
        val cbounds = typeParam.cbounds.visit
        val vbounds = typeParam.vbounds.visit
        val tbounds = typeParam.tbounds.visit

        f"$mods $name $cbounds $vbounds $tbounds"
      }

      case typeBounds: Type.Bounds => {
        val hi = typeBounds.hi.map(x => f"upper bound ${x.visit}").getOrElse("")
        val lo = typeBounds.hi.map(x => f"lower bound ${x.visit}").getOrElse("")

        f"$hi $lo"
      }

      case defnClass: Defn.Class => {
        val ctor = defnClass.ctor.visit
        val mods = defnClass.mods.visit
        val templ = defnClass.templ.visit
        val name = defnClass.name.visit
        val tparams = defnClass.tparams.visit

        f"$mods class $name $templ $ctor"
      }

      case modCase: Mod.Case => {
        "case"
      }

      case ctorPrimary: Ctor.Primary => {
        val mods = ctorPrimary.mods.visit
        val name = ctorPrimary.name.visit
        val paramss = ctorPrimary.paramss

        s"$mods $name"
      }


      case x => f"[?] ${x.productPrefix} [/?]"
    }
  }

  def recursiveExplore(stats: List[Tree])(level: Int): Unit = {
    println(stats.map(_.visit).mkString("\n"))
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

