package part2.chapter9

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def advanceBy(n: Int): Location = copy(offset = offset+n)

  def toError(msg: String): ParseError = ParseError(List((this, msg)))
}