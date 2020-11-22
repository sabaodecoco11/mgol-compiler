package Utils

import Enum.Token
import Enum.Token.Token

import scala.collection.immutable.HashMap

object Common {

  type Lexeme = String
  type SymbolTable = HashMap[Lexeme, Tuple2[Token, String]]

  //abaixo a tabela de símbolos está sendo populada com as palavras reservadas,
  // estas que possuem lexemas iguais aos tokens
  def getSymbolTable: HashMap[Lexeme, Tuple2[Token, String]] = {
      HashMap[Lexeme, Tuple2[Token, String]](
        Token.R_INIT -> (Token.R_INIT, ""),
        Token.R_END -> (Token.R_END, ""),

        Token.R_VAR_INIT -> (Token.R_VAR_INIT, ""),
        Token.R_VAR_END -> (Token.R_VAR_END, ""),

        Token.R_WRITE -> (Token.R_WRITE, ""),
        Token.R_READ -> (Token.R_READ, ""),

        Token.R_IF_INIT -> (Token.R_IF_INIT -> ""),
        Token.R_THEN -> (Token.R_THEN , ""),
        Token.R_IF_END -> (Token.R_IF_END , ""),


        Token.R_ID_INT -> (Token.R_ID_INT , "int"),
        Token.R_ID_LIT -> (Token.R_ID_LIT, "literal"),
        Token.R_ID_REAL -> (Token.R_ID_REAL, "real"),
    )

  }
//  def getSymbolTable(lexeme: Lexeme, token: Token, t: String): HashMap[Lexeme, Map[Token, Object]] = {
//    HashMap[Lexeme, Map[Token, String]](
//      lexeme -> Map(token -> t)
//    )
//  }
}
