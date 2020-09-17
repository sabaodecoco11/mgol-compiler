package Utils

import Enum.Token
import Enum.Token.Token

import scala.collection.immutable.HashMap

object SymbolTable {

  type Lexeme = String

  //abaixo a tabela de símbolos está sendo populada com as palavras reservadas,
  // estas que possuem lexemas iguais aos tokens
  def get(): HashMap[Lexeme, Map[Token, Object]] = {
      HashMap[Lexeme, Map[Token, Object]](
        Token.R_INIT -> Map[Token, Object](
          Token.R_INIT -> ""),
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
        Token.R_ID_END -> Map[Token, Object](
          Token.R_ID_END -> ""),
        Token.R_ID_INT -> Map[Token, Object](
          Token.R_ID_INT -> ""),
        Token.R_ID_LIT -> Map[Token, Object](
          Token.R_ID_LIT -> ""),
        Token.R_ID_REAL -> Map[Token, Object](
          Token.R_ID_REAL -> ""),
    )

  }
}
