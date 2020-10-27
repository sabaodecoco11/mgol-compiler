package Enum

object Token extends Enumeration {
  type Token = String

  val NUMBER = "num"
  val ID = "id"
  val LITERAL = "literal"
  val END_OF_FILE = "$"
  val REL_OPERATOR = "OPR"
  val MATH_OPERATOR = "OPM"
  val OP_PARENTHESIS = "AB_P"
  val CL_PARENTHESIS = "FC_P"
  val SEMICOLON = "PT_V"
  val ERROR = "ERRO"
  val COMMENT = "Comentário"
  val ATTR = "RCB"

  //Tokens para palavras reservadas
  val R_INIT = "inicio"
  val R_VAR_INIT = "varinicio"
  val R_VAR_END = "varfim"
  val R_WRITE = "escreva"
  val R_READ = "leia"
  val R_IF_INIT = "se"
  val R_THEN = "entao"
  val R_IF_END = "fimse"
  val R_ID_INT = "inteiro"
  val R_ID_LIT = "lit"
  val R_ID_REAL = "real"
  val R_END = "fim"

}
