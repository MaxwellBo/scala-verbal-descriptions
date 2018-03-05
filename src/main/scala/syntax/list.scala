package syntax

object list {
  implicit class ListExtensions[A](self: List[A]) {
    private def suffix = self.length match {
      case 0 => "s"
      case 1 => ": "
      case _ => "s: "
    }

    def count(name: String): Option[String] = {
      if (self.isEmpty) {
        None
      } else {
        Some { countEmpty(name) }
      }
    }

    def countEmpty(name: String): String = {
      s"${self.length} $name$suffix" + self.commas.getOrElse("")
    }

    def spaces: Option[String] = {
      if (self.isEmpty) {
        None
      } else {
        Some { self.mkString(" ") }
      }
    }

    def lines: Option[String] = {
      if (self.isEmpty) {
        None
      } else {
        Some { self.mkString("\n") }
      }
    }

    def commas: Option[String] = {
      if (self.isEmpty) {
        None
      } else {
        Some { self.init.mkString(", ") + " and " + self.last } // safe
      }
    }
  }
  implicit class OptionListExtensions[A](self: List[Option[A]]) {
    def somes: List[A] = self.collect {
      case Some(x) => x
    }
  }
}
