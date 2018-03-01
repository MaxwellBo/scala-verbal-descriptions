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
  def toRange(input: Input): Position.Range = Position.Range(input, ???, ???)
}

final class LocationRange(
  from: Location,
  to: Location
) {
  def toRange(input: Input): Position.Range = Position.Range(input, ???, ???)
}

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
    def quote = f"\'$self\'"
  }

  implicit class TreeOps(self: Tree) {
    def visit: String = visitTree(self)
  }

  implicit class ListOps(self: List[Tree]) {
    private def formatList[A](sep: String)(xs: List[A]): String = {
      if (xs.length <= 1) {
        xs.mkString(sep) // empty lists are going to produce empty strings
      } else {
        xs.init.mkString(sep) + sep + " and " + xs.last // safe
      }
    }

    def suffix: String = {
      self.length match {
        case 0 => "s"
        case 1 => ": "
        case _ => "s: "
      }
    }

    private def visitList[A](xs: List[Tree]): String = {
      formatList(", ")(xs.map(_.visit))
    }

    private def visitListNewLines[A](xs: List[Tree]): String = {
      formatList(";")(xs.map(_.visit))
    }

    private def visitListOptional(xs: List[Tree], formatString: String => String = identity): String = {
      if (xs.isEmpty) {
        ""
      } else {
        formatString(visitList(xs))
      }
    }

    def visit: String = visitList(self)
    def visitNewLines: String = visitListNewLines(self)
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
    // matching the order as defined by package scala.meta.Trees
    
    tree match {
      // ################################################################### //
      // @branch trait Name extends Ref { def value: String }

      // @ast class Anonymous() extends Name
      case nameAnonymous: Name.Anonymous => "TODO"

      // @ast class Indeterminate(value: Predef.String @nonEmpty) extends Name
      case nameIndeterminate: Name.Indeterminate => "TODO"

      // ################################################################### //
      // @branch trait Lit extends Term with Pat with Type { def value: Any }

      // @ast class Null() extends Lit { def value: Any = null }
      case litNull: Lit.Null => "null"

      // @ast class Int(value: scala.Int) extends Lit
      // @ast class Double(format: scala.Predef.String) extends Lit { val value = format.toDouble }
      // @ast class Float(format: scala.Predef.String) extends Lit { val value = format.toFloat }
      // @ast class Byte(value: scala.Byte) extends Lit
      // @ast class Char(value: scala.Char) extends Lit
      // @ast class Long(value: scala.Long) extends Lit
      // @ast class Boolean(value: scala.Boolean) extends Lit
      case litInt: Lit.Int => f"int ${litInt.value}"
      case litDouble: Lit.Double => f"double ${litDouble.value}"
      case litFloat: Lit.Float => f"float ${litFloat.value}"
      case litByte: Lit.Byte => f"byte ${litByte.value}"
      case litChar: Lit.Char => f"char ${litChar.value}"
      case litLong: Lit.Long => f"long ${litLong.value}"
      case litBoolean: Lit.Boolean => f"boolean ${litBoolean.value}"

      // @ast class Unit() extends Lit { def value: Any = () }
      case litUnit: Lit.Unit => "Unit"

      // @ast class String(value: scala.Predef.String) extends Lit
      case litString: Lit.String => f"string {litString.value}"

      // @ast class Symbol(value: scala.Symbol) extends Lit
      case litSymbol: Lit.Symbol => "TODO"

      // ################################################################### //
        
      // @branch trait Term extends Stat
        
      // @ast class This(qual: scala.meta.Name) extends Term.Ref
      case termThis: Term.This => {
        // TODO: Children
        "this"
      }

      // @ast class Super(thisp: scala.meta.Name, superp: scala.meta.Name) extends Term.Ref
      case termSuper: Term.Super => {
        // TODO: Children
        "super"
      }

      // @ast class Name(value: Predef.String @nonEmpty) extends scala.meta.Name with Term.Ref with Pat
      case termName: Term.Name => termName.value

      // @ast class Select(qual: Term, name: Term.Name) extends Term.Ref with Pat
      case termSelect: Term.Select => {
        // TODO: children
        // I actually have no idea what this production is ¯\_("/)_/¯ ~mbo
        "select" 
      }

      // @ast class Interpolate(prefix: Name, parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
      //   checkFields(parts.length == args.length + 1)
      // }
      case termInterpolate: Term.Interpolate => {
        // TODO: Children
        "interpolate"
      }

      // @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
      //   checkFields(parts.length == args.length + 1)
      // }
      case termXml: Term.Xml => {
        // TODO: Children
        "xml"
      }
      // @ast class Apply(fun: Term, args: List[Term]) extends Term
      case termApply: Term.Apply => {
        // TODO: Children
        "Apply"
      }

      // @ast class ApplyType(fun: Term, targs: List[Type] @nonEmpty) extends Term
      case termApplyType: Term.ApplyType => {
        // TODO: Children
        "ApplyType"
      }

      // @ast class ApplyInfix(lhs: Term, op: Name, targs: List[Type], args: List[Term]) extends Term
      case termApplyInfix: Term.ApplyInfix => {
        // TODO: Children
        "ApplyInfix"
      }

      // @ast class ApplyUnary(op: Name, arg: Term) extends Term.Ref {
      //   checkFields(op.isUnaryOp)
      // }
      case termApplyUnary: Term.ApplyUnary => {
        // TODO: Children
        "ApplyUnary"
      }

      // @ast class Assign(lhs: Term, rhs: Term) extends Term {
      //   checkFields(lhs.is[Term.Quasi] || lhs.is[Term.Ref] || lhs.is[Term.Apply])
      //   checkParent(ParentChecks.TermAssign)
      // }
      case termAssign: Term.Assign => {
        // TODO: Children
        "Assign"
      }

      // @ast class Return(expr: Term) extends Term
      case termReturn: Term.Return => {
        // TODO: Children
        "Return"
      }

      // @ast class Throw(expr: Term) extends Term
      case termThrow: Term.Throw => {
        // TODO: Children
        "Throw"
      }

      // @ast class Ascribe(expr: Term, tpe: Type) extends Term
      case termAscribe: Term.Ascribe => {
        // TODO: Children
        "Ascribe"
      }

      // @ast class Annotate(expr: Term, annots: List[Mod.Annot] @nonEmpty) extends Term
      case termAnnotate: Term.Annotate => {
        // TODO: Children
        "Annotate"
      }

      // @ast class Tuple(args: List[Term] @nonEmpty) extends Term {
      //   // tuple must have more than one element
      //   // however, this element may be Quasi with "hidden" list of elements inside
      //   checkFields(args.length > 1 || (args.length == 1 && args.head.is[Term.Quasi]))
      // }
      case termTuple: Term.Tuple => {
        // TODO: Children
        "Tuple"
      }

      // @ast class Block(stats: List[Stat]) extends Term {
      //   checkFields(stats.forall(_.isBlockStat))
      // }
	  case termBlock: Term.Block => {
        termBlock.stats.visitNewLines
      }

      // @ast class If(cond: Term, thenp: Term, elsep: Term) extends Term
      case termIf: Term.If => {
        // TODO: Children
        "If"
      }

      // @ast class Match(expr: Term, cases: List[Case] @nonEmpty) extends Term
      case termMatch: Term.Match => {
        // TODO: Children
        "Match"
      }

      // @ast class Try(expr: Term, catchp: List[Case], finallyp: Option[Term]) extends Term
      case termTry: Term.Try => {
        // TODO: Children
        "Try"
      }

      // @ast class TryWithHandler(expr: Term, catchp: Term, finallyp: Option[Term]) extends Term
      case termTryWithHandler: Term.TryWithHandler => {
        // TODO: Children
        "TryWithHandler"
      }

      // @ast class Function(params: List[Term.Param], body: Term) extends Term {
      //   checkFields(params.forall(param => param.is[Term.Param.Quasi] || (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)))
      //   checkFields(params.exists(_.is[Term.Param.Quasi]) || params.exists(_.mods.exists(_.is[Mod.Implicit])) ==> (params.length == 1))
      // }
      case termFunction: Term.Function => {
        // TODO: Children
        "Function"
      }

      // @ast class PartialFunction(cases: List[Case] @nonEmpty) extends Term
      case termPartialFunction: Term.PartialFunction => {
        // TODO: Children
        "PartialFunction"
      }

      // @ast class While(expr: Term, body: Term) extends Term
      case termWhile: Term.While => {
        // TODO: Children
        "While"
      }

      // @ast class Do(body: Term, expr: Term) extends Term
      case termDo: Term.Do => {
        // TODO: Children
        "Do"
      }

      // @ast class For(enums: List[Enumerator] @nonEmpty, body: Term) extends Term {
      //   checkFields(enums.head.is[Enumerator.Generator] || enums.head.is[Enumerator.Quasi])
      // }
      case termFor: Term.For => {
        // TODO: Children
        "For"
      }

      // @ast class ForYield(enums: List[Enumerator] @nonEmpty, body: Term) extends Term
      case termForYield: Term.ForYield => {
        // TODO: Children
        "ForYield"
      }

      // @ast class New(init: Init) extends Term
      case termNew: Term.New => {
        // TODO: Children
        "New"
      }

      // @ast class NewAnonymous(templ: Template) extends Term
      case termNewAnonymous: Term.NewAnonymous => {
        // TODO: Children
        "NewAnonymous"
      }

      // @ast class Placeholder() extends Term
      case termPlaceholder: Term.Placeholder => {
        // TODO: Children
        "Placeholder"
      }

      // @ast class Eta(expr: Term) extends Term
      case termEta: Term.Eta => {
        // TODO: Children
        "Eta"
      }

      // @ast class Repeated(expr: Term) extends Term {
      //   checkParent(ParentChecks.TermRepeated)
      // }
      case termRepeated: Term.Repeated => {
        // TODO: Children
        "Repeated"
      }

      // @ast class Param(mods: List[Mod], name: meta.Name, decltpe: Option[Type], default: Option[Term]) extends Member
      case termParam: Term.Param => {
        val name = termParam.name.visit
        val decltpe = termParam.decltpe.map(x => f"${x.visit}").getOrElse("")
        val default = termParam.default.map(x => f"with default value ${x.visit}").getOrElse("")
        val mods = termParam.mods.visit

        f"$mods $name $decltpe $default"
      }

      // ################################################################### //
      // @branch trait Ref extends Type with scala.meta.Ref

      // @ast class Name(value: String @nonEmpty) extends scala.meta.Name with Type.Ref
      case typeName: Type.Name => {
        typeName.value
      }

      // @ast class Select(qual: Term.Ref, name: Type.Name) extends Type.Ref {
      //   checkFields(qual.isPath || qual.is[Term.Super] || qual.is[Term.Ref.Quasi])
      // }
      case typeSelect: Type.Select => {
        /// TODO: Children

        "Select"
      }

      // @ast class Project(qual: Type, name: Type.Name) extends Type.Ref
      case typeProject: Type.Project => {
        /// TODO: Children

        "Project"
      }

      // @ast class Singleton(ref: Term.Ref) extends Type.Ref {
      //   checkFields(ref.isPath || ref.is[Term.Super])
      // }
      case typeSingleton: Type.Singleton => {
        /// TODO: Children

        "Singleton"
      }

      // @ast class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
      case typeApply: Type.Apply => {
        val tpe = typeApply.tpe.visit
        val args = typeApply.args.visit

        s"$tpe of $args"
      }

      // @ast class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
      case typeApplyInfix: Type.ApplyInfix => {
        /// TODO: Children

        "ApplyInfix"
      }

      // @ast class Function(params: List[Type], res: Type) extends Type
      case typeFunction: Type.Function => {
        /// TODO: Children

        "Function"
      }

      // @ast class ImplicitFunction(params: List[Type], res: Type) extends Type
      case typeImplicitFunction: Type.ImplicitFunction => {
        /// TODO: Children

        "ImplicitFunction"
      }

      // @ast class Tuple(args: List[Type] @nonEmpty) extends Type {
      //   checkFields(args.length > 1 || (args.length == 1 && args.head.is[Type.Quasi]))
      // }
      case typeTuple: Type.Tuple => {
        /// TODO: Children

        "Tuple"
      }

      // @ast class With(lhs: Type, rhs: Type) extends Type
      case typeWith: Type.With => {
        /// TODO: Children

        "With"
      }

      // @ast class And(lhs: Type, rhs: Type) extends Type
      case typeAnd: Type.And => {
        /// TODO: Children

        "And"
      }

      // @ast class Or(lhs: Type, rhs: Type) extends Type
      case typeOr: Type.Or => {
        /// TODO: Children

        "Or"
      }

      // @ast class Refine(tpe: Option[Type], stats: List[Stat]) extends Type {
      //   checkFields(stats.forall(_.isRefineStat))
      // }
      case typeRefine: Type.Refine => {
        /// TODO: Children

        "Refine"
      }

      // @ast class Existential(tpe: Type, stats: List[Stat] @nonEmpty) extends Type {
      //   checkFields(stats.forall(_.isExistentialStat))
      // }
      case typeExistential: Type.Existential => {
        /// TODO: Children

        "Existential"
      }

      // @ast class Annotate(tpe: Type, annots: List[Mod.Annot] @nonEmpty) extends Type
      case typeAnnotate: Type.Annotate => {
        /// TODO: Children

        "Annotate"
      }

      // @ast class Lambda(tparams: List[Type.Param], tpe: Type) extends Type {
      //   checkParent(ParentChecks.TypeLambda)
      // }
      case typeLambda: Type.Lambda => {
        /// TODO: Children

        "Lambda"
      }

      // @ast class Method(paramss: List[List[Term.Param]], tpe: Type) extends Type {
      //   checkParent(ParentChecks.TypeMethod)
      // }
      case typeMethod: Type.Method => {
        /// TODO: Children

        "Method"
      }

      // @ast class Placeholder(bounds: Bounds) extends Type
      case typePlaceholder: Type.Placeholder => {
        /// TODO: Children

        "Placeholder"
      }

      // @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
      case typeBounds: Type.Bounds => {
        val hi = typeBounds.hi.map(x => f"upper bound ${x.visit}").getOrElse("")
        val lo = typeBounds.hi.map(x => f"lower bound ${x.visit}").getOrElse("")

        f"$hi $lo"
      }

      // @ast class ByName(tpe: Type) extends Type {
      //   checkParent(ParentChecks.TypeByName)
      // }
      case typeByName: Type.ByName => {
        /// TODO: Children

        "ByName"
      }

      // @ast class Repeated(tpe: Type) extends Type {
      //   checkParent(ParentChecks.TypeRepeated)
      // }
      case typeRepeated: Type.Repeated => {
        /// TODO: Children

        "Repeated"
      }

      // @ast class Var(name: Name) extends Type with Member.Type {
      //   checkFields(name.value(0).isLower)
      //   checkParent(ParentChecks.TypeVar)
      // }
      case typeVar: Type.Var => {
        /// TODO: Children

        "Var"
      }

      // @ast class Param(mods: List[Mod],
      //                 name: meta.Name,
      //                 tparams: List[Type.Param],
      //                 tbounds: Type.Bounds,
      //                 vbounds: List[Type],
      //                 cbounds: List[Type]) extends Member
      case typeParam: Type.Param => {
        val name = typeParam.name.visit
        val mods = typeParam.mods.visit
        val cbounds = typeParam.cbounds.visitFormatNonEmpty(x => f"bounded by $x")
        val vbounds = typeParam.vbounds.visitFormatNonEmpty(x => f"vbounds $x")
        val tbounds = typeParam.tbounds.visit

        f"$mods $name $cbounds $vbounds $tbounds"
      }

      // ################################################################### //
      // @branch trait Pat extends Tree

      // @ast class Var(name: scala.meta.Term.Name) extends Pat with Member.Term {
      //   // NOTE: can't do this check here because of things like `val X = 2`
      //   // checkFields(name.value(0).isLower)
      //   checkParent(ParentChecks.PatVar)
      // }
      case patVar: Pat.Var => {
        val name = patVar.name.visit.quote

        f"$name"
      }

      // @ast class Wildcard() extends Pat
      case patWildcard: Pat.Wildcard => {
       // TODO: Children
       
        "Wildcard"
      }

      // @ast class SeqWildcard() extends Pat {
      //   checkParent(ParentChecks.PatSeqWildcard)
      // }
      case patSeqWildcard: Pat.SeqWildcard => {
       // TODO: Children
       
        "SeqWildcard"
      }

      // @ast class Bind(lhs: Pat, rhs: Pat) extends Pat {
      //   checkFields(lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
      // }
      case patBind: Pat.Bind => {
       // TODO: Children
       
        "Bind"
      }

      // @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
      case patAlternative: Pat.Alternative => {
       // TODO: Children
       
        "Alternative"
      }

      // @ast class Tuple(args: List[Pat] @nonEmpty) extends Pat {
      //   checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat.Quasi]))
      // }
      case patTuple: Pat.Tuple => {
       // TODO: Children
       
        "Tuple"
      }

      // @ast class Extract(fun: Term, args: List[Pat]) extends Pat {
      //   checkFields(fun.isExtractor)
      // }
      case patExtract: Pat.Extract => {
       // TODO: Children
       
        "Extract"
      }

      // @ast class ExtractInfix(lhs: Pat, op: Term.Name, rhs: List[Pat]) extends Pat
      // @ast class Interpolate(prefix: Term.Name, parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
      //   checkFields(parts.length == args.length + 1)
      // }
      case patExtractInfix: Pat.ExtractInfix => {
       // TODO: Children
       
        "ExtractInfix"
      }

      // @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
      //   checkFields(parts.length == args.length + 1)
      // }
      case patXml: Pat.Xml => {
       // TODO: Children
       
        "Xml"
      }

      // @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
      //   checkFields(lhs.is[Pat.Wildcard] || lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
      //   checkFields(!rhs.is[Type.Var] && !rhs.is[Type.Placeholder])
      // }
      case patTyped: Pat.Typed => {
       // TODO: Children
       
        "Typed"
      }

      // ################################################################### //
      // @branch trait Decl extends Stat

      // @ast class Val(mods: List[Mod],
      //                pats: List[Pat] @nonEmpty,
      //                decltpe: scala.meta.Type) extends Decl
      case declVal: Decl.Val => {
       // TODO: Children
       
        "Val"
      }

      // @ast class Var(mods: List[Mod],
      //                pats: List[Pat] @nonEmpty,
      //                decltpe: scala.meta.Type) extends Decl
      case declVar: Decl.Var => {
       // TODO: Children
       
        "Var"
      }
      
      // @ast class Def(mods: List[Mod],
      //                name: Term.Name,
      //                tparams: List[scala.meta.Type.Param],
      //                paramss: List[List[Term.Param]],
      //                decltpe: scala.meta.Type) extends Decl with Member.Term
      case declDef: Decl.Def => {
       // TODO: Children
       
        "Def"
      }
      
      // @ast class Type(mods: List[Mod],
      //                 name: scala.meta.Type.Name,
      //                 tparams: List[scala.meta.Type.Param],
      //                 bounds: scala.meta.Type.Bounds) extends Decl with Member.Type
      case declType: Decl.Type => {
       // TODO: Children
       
        "Type"
      }

      // ################################################################### //
      // @branch trait Defn extends Stat

      // @ast class Val(mods: List[Mod],
      //                pats: List[Pat] @nonEmpty,
      //                decltpe: Option[scala.meta.Type],
      //                rhs: Term) extends Defn {
      //   checkFields(pats.forall(!_.is[Term.Name]))
      // }
      case defnVal: Defn.Val => {
        val mods = defnVal.mods.visit
        val rhs = defnVal.rhs.visit
        val pats = defnVal.pats.visit
        val decltpe = defnVal.decltpe.map(x => f"with return type ${x.visit}").getOrElse("")

        f"$mods $pats $decltpe with $rhs"
      }

      // @ast class Var(mods: List[Mod],
      //                pats: List[Pat] @nonEmpty,
      //                decltpe: Option[scala.meta.Type],
      //                rhs: Option[Term]) extends Defn {
      //   checkFields(pats.forall(!_.is[Term.Name]))
      //   checkFields(decltpe.nonEmpty || rhs.nonEmpty)
      //   checkFields(rhs.isEmpty ==> pats.forall(_.is[Pat.Var]))
      // }
      case defnVar: Defn.Var => {
        val mods = defnVar.mods.visit
        val rhs = defnVar.rhs.map(_.visit).getOrElse("")
        val pats = defnVar.pats.visit
        val decltpe = defnVar.decltpe.map(x => f"with return type ${x.visit}").getOrElse("")

        f"$mods $pats $decltpe with $rhs"
      }


      // @ast class Def(mods: List[Mod],
      //                name: Term.Name,
      //                tparams: List[scala.meta.Type.Param],
      //                paramss: List[List[Term.Param]],
      //                decltpe: Option[scala.meta.Type],
      //                body: Term) extends Defn with Member.Term
      case defnDef: Defn.Def => {
        val body = defnDef.body.visit
        val decltpe = defnDef.decltpe.map(x => f"with return type ${x.visit}").getOrElse("")
        val mods = defnDef.mods.visit
        val name = defnDef.name.visit
        val tparams = defnDef.tparams.visit
        val paramss = defnDef.paramss.flatten.visit

        f"\n$mods definition $name with $tparams and params $paramss $decltpe $body"
      }
      // @ast class Macro(mods: List[Mod],
      //                  name: Term.Name,
      //                  tparams: List[scala.meta.Type.Param],
      //                  paramss: List[List[Term.Param]],
      //                  decltpe: Option[scala.meta.Type],
      //                  body: Term) extends Defn with Member.Term
    case defnMacro: Defn.Macro => {
      // TODO: Children

      "Macro"
    }

      // @ast class Type(mods: List[Mod],
      //                 name: scala.meta.Type.Name,
      //                 tparams: List[scala.meta.Type.Param],
      //                 body: scala.meta.Type) extends Defn with Member.Type
      case defnType: Defn.Type => {
        val name = defnType.name.visit
        val body = defnType.body.visit
        val tparams = defnType.tparams.visit
        val mods = defnType.mods.visit

        f"$mods type alias $name type $tparams equal to $body"
      }

      // @ast class Class(mods: List[Mod],
      //                  name: scala.meta.Type.Name,
      //                  tparams: List[scala.meta.Type.Param],
      //                  ctor: Ctor.Primary,
      //                  templ: Template) extends Defn with Member.Type
      case defnClass: Defn.Class => {
        val ctor = defnClass.ctor.visit
        val mods = defnClass.mods.visit
        val templ = defnClass.templ.visit
        val name = defnClass.name.visit
        val tparams = defnClass.tparams.visit

        f"$mods class $name $templ $ctor"
      }
      // @ast class Trait(mods: List[Mod],
      //                  name: scala.meta.Type.Name,
      //                  tparams: List[scala.meta.Type.Param],
      //                  ctor: Ctor.Primary,
      //                  templ: Template) extends Defn with Member.Type {
      //   checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
      // }
      case defnTrait: Defn.Trait => {
        val name = defnTrait.name.visit
        val tparams = defnTrait.tparams.visitFormatNonEmpty(x => f"has type params $x")
        val mods = defnTrait.mods.visit
        val ctor = defnTrait.ctor.visit
        val template = defnTrait.templ.visit

        f"$mods trait $name $tparams $template $ctor"
      }

      // @ast class Object(mods: List[Mod],
      //                   name: Term.Name,
      //                   templ: Template) extends Defn with Member.Term {
      //   checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
      // }
      case defnObject: Defn.Object => {
        val mods = defnObject.mods.visit
        val name = defnObject.name.visit
        val templ = defnObject.templ.visit

        f"$mods object $name $templ"
      }

      // ################################################################### //
      // @ast class Pkg(ref: Term.Ref, stats: List[Stat])
      //  extends Member.Term with Stat {
      //   checkFields(ref.isQualId)
      //   checkFields(stats.forall(_.isTopLevelStat))
      //   def name: Term.Name = ref match {
      //     case name: Term.Name => name
      //     case Term.Select(_, name: Term.Name) => name
      //   }
      // }
      case pkg: Pkg => {
        // TODO: Children

        "Pkg"
      }

      // @ast class Object(mods: List[Mod], name: Term.Name, templ: Template)
      //     extends Member.Term with Stat {
      //   checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
      case pkgObject: Pkg.Object => {
        // TODO: Children

        "PkgObject"
      }


      // ################################################################### //
      // @branch trait Ctor extends Tree with Member

      // @ast class Primary(mods: List[Mod],
      //              name: Name,
      //              paramss: List[List[Term.Param]]) extends Ctor
      case ctorPrimary: Ctor.Primary => {
        val mods = ctorPrimary.mods.visit
        val name = ctorPrimary.name.visit
        val paramss = ctorPrimary.paramss.flatten.visitFormatNonEmpty(x => f"contains $x")

        s"$mods $name $paramss"
      }

      // @ast class Secondary(mods: List[Mod],
      //                name: Name,
      //                paramss: List[List[Term.Param]] @nonEmpty,
      //                init: Init,
      //                stats: List[Stat]) extends Ctor with Stat {
      //   checkFields(stats.forall(_.isBlockStat))
      // }
      case ctorSecondary: Ctor.Secondary => {
        // TODO: Children

        "CtorPrimary"
      }

      // @ast class Init(tpe: Type, name: Name, argss: List[List[Term]]) extends Ref {
      //   checkFields(tpe.isConstructable)
      //   checkParent(ParentChecks.Init)
      // }
      case init: Init => {
        val name = init.name.visit
        val tpe = init.tpe.visit
        val argss = init.argss

        f"$name extending $tpe"
      }

      // @ast class Self(name: Name, decltpe: Option[Type]) extends Member
      case self: Self => {
        val name = self.name.visit
        val decltpe = self.decltpe.map(x => f"with return type ${x.visit}").getOrElse("")

        s"$name $decltpe"
      }

      // @ast class Template(early: List[Stat],
      //                     inits: List[Init],
      //                     self: Self,
      //                     stats: List[Stat]) extends Tree {
      //   checkFields(early.forall(_.isEarlyStat && inits.nonEmpty))
      //   checkFields(stats.forall(_.isTemplateStat))
      // }
      case template: Template => {
        val early = template.early.visit
        val inits = template.inits.visit
        val self = template.self.visit
        val stats = template.stats.visitFormatNonEmpty { x =>
          f"has ${template.stats.length} declaration${template.stats.suffix} $x"
        }

        s"$early $inits $self $stats"
      }

      // ################################################################### //
      // @branch trait Mod extends Tree
      // @ast class Annot(init: Init) extends Mod {
      //   @deprecated("Use init instead", "1.9.0")
      //   def body = init
      // }
      case modAnnot: Mod.Annot => {
        // TODO: Children
        "Annot"
      }

      // @ast class Private(within: Ref) extends Mod {
      //   checkFields(within.isWithin)
      // }
      // @ast class Protected(within: Ref) extends Mod {
      //   checkFields(within.isWithin)
      // }
      case modPrivate: Mod.Private => {
        "private"
      }

      case modProtected: Mod.Protected => {
        "protected"
      }

      // @ast class Implicit() extends Mod
      // @ast class Final() extends Mod
      // @ast class Sealed() extends Mod
      // @ast class Override() extends Mod
      // @ast class Case() extends Mod
      // @ast class Abstract() extends Mod
      // @ast class Covariant() extends Mod
      // @ast class Contravariant() extends Mod
      // @ast class Lazy() extends Mod
      // @ast class ValParam() extends Mod
      // @ast class VarParam() extends Mod
      // @ast class Inline() extends Mod 
      case modImplicit: Mod.Implicit => "implicit"
      case modFinal: Mod.Final => "final"
      case modSealed: Mod.Sealed => "sealed"
      case modOverride: Mod.Override => "override"
      case modCase: Mod.Case => "case"
      case modAbstract: Mod.Abstract => "abstract"
      case modCovariant: Mod.Covariant => "covariant"
      case modContravariant: Mod.Contravariant => "contravariant"
      case modLazy: Mod.Lazy => "lazy"
      case modValParam: Mod.ValParam => "val param"
      case modVarParam: Mod.VarParam => "var param"
      case modInline: Mod.Inline => "inline"


      // @ast class Import(importers: List[Importer] @nonEmpty) extends Stat
      case _import: Import => {
        // TODO: Children 
        "An import"
      }

      // @ast class Importer(ref: Term.Ref, importees: List[Importee] @nonEmpty) extends Tree {
      //   checkFields(ref.isStableId)
      // }
      case importer: Importer => {
        // TODO: Children

        "Importer"
      }

      // ################################################################### //
      // @branch trait Importee extends Tree with Ref

      // @ast class Wildcard() extends Importee
      case importeeWildcard: Importee.Wildcard => {
        "Wildcard"
      }

      // @ast class Name(name: scala.meta.Name) extends Importee {
      //   checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
      // }
      case importeeName: Importee.Name => {
        // TODO: Children

        "Name"
      }

      // @ast class Rename(name: scala.meta.Name, rename: scala.meta.Name) extends Importee {
      //   checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
      //   checkFields(rename.is[scala.meta.Name.Quasi] || rename.is[scala.meta.Name.Indeterminate])
      // }
      case importeeRename: Importee.Rename => {
        // TODO: Children

        "Rename"
      }
      
      // @ast class Unimport(name: scala.meta.Name) extends Importee {
      //   checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
      // }
      case importeeUnimport: Importee.Unimport => {
        // TODO: Children

        "Unimport"
      }

      // @ast class Case(pat: Pat, cond: Option[Term], body: Term) extends Tree
      case _case: Case => {
        // TODO: Children

        "case"
      }

      // @ast class Source(stats: List[Stat]) extends Tree {}
      case source: Source => {
        // TODO: Children

        "source"
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

