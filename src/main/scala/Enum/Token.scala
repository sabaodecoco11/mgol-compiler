package Enum

object Token extends Enumeration {
  type Token = String

  val NUMBER = "num"
  val ID = "id"
  val LITERAL = "Literal"
  val END_OF_FILE = "EOF"
  val REL_OPERATOR = "OPR"
  val MATH_OPERATOR = "OPM"
  val OP_PARENTHESIS = "AB_P"
  val CL_PARENTHESIS = "FC_P"
  val SEMICOLON = "PT_V"
  val ERROR = "ERRO"
  val COMMENT = "Comentário"
  val ATTR = "RCB"
}
