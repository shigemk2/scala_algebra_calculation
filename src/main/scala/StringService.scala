class StringService {
  def concat(str: String, strings: String*): String =
    str + strings.mkString

  def trim(str: String): String =
    str.trim
}
