package syntax

object option {
  implicit class OptionStringExtensions[A](self: Option[String]) {
    def prefix(name: String): Option[String] = self.map(contents => s"$name $contents")
  }
}
