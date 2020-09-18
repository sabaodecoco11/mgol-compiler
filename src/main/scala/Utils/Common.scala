package Utils

import Enum.Token
import Enum.Token.Token

import scala.collection.immutable.HashMap

object Common {

  type Lexeme = String
  type SymbolTable = HashMap[Lexeme, Map[Token, Object]]

  //abaixo a tabela de símbolos está sendo populada com as palavras reservadas,
  // estas que possuem lexemas iguais aos tokens
  def getSymbolTable(): HashMap[Lexeme, Map[Token, Object]] = {
      HashMap[Lexeme, Map[Token, Object]](
        Token.R_INIT -> Map[Token, Object](
          Token.R_INIT -> ""),
        Token.R_END -> Map[Token, Object](
          Token.R_END -> ""),

        Token.R_VAR_INIT -> Map[Token, Object](
          Token.R_VAR_INIT -> ""),
        Token.R_VAR_END -> Map[Token, Object](
          Token.R_VAR_END -> ""),

        Token.R_WRITE -> Map[Token, Object](
          Token.R_WRITE -> ""),
        Token.R_READ -> Map[Token, Object](
          Token.R_READ -> ""),

        Token.R_IF_INIT -> Map[Token, Object](
          Token.R_IF_INIT -> ""),
        Token.R_THEN -> Map[Token, Object](
          Token.R_THEN -> ""),
        Token.R_IF_END -> Map[Token, Object](
          Token.R_IF_END -> ""),


        Token.R_ID_INT -> Map[Token, Object](
          Token.R_ID_INT -> ""),
        Token.R_ID_LIT -> Map[Token, Object](
          Token.R_ID_LIT -> ""),
        Token.R_ID_REAL -> Map[Token, Object](
          Token.R_ID_REAL -> ""),
    )

  }
  def getSymbolTable(lexeme: Lexeme, token: Token, t: String): HashMap[Lexeme, Map[Token, Object]] = {
    HashMap[Lexeme, Map[Token, String]](
      lexeme -> Map(token -> t)
    )
  }
}
