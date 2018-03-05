package syntax

object string {
  implicit class StringExtensions(self: String) {
    def quote = f"\'$self\'"
  }
}
