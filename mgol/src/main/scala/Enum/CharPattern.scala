package Enum

object CharPattern extends Enumeration {
  type CharPattern = String

  val LOGICAL = "logical"
  val NUMERIC = "number"
  val LETTER = "letter"
  val IGNORED = "ignored"
}
