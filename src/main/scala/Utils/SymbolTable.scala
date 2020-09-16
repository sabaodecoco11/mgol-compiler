package Utils

import Enum.Token
import Enum.Token.Token

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object SymbolTable {

  type Lexeme = String

  def get(): HashMap[Lexeme, Map[Token, Object]] = {
      HashMap[Lexeme, Map[Token, Object]](
        "varinicio" -> Map[Token, Object](
          Token.ID -> ""),
        "escreva" -> Map[Token, Object](
          Token.ID -> ""),
        "leia" -> Map[Token, Object](
          Token.ID -> ""),
        "se" -> Map[Token, Object](
          Token.ID -> ""),
        "entao" -> Map[Token, Object](
          Token.ID -> ""),
        "fimse" -> Map[Token, Object](
          Token.ID -> ""),
        "fim" -> Map[Token, Object](
          Token.ID -> ""),
        "inteiro" -> Map[Token, Object](
          Token.ID -> ""),
        "lit" -> Map[Token, Object](
          Token.ID -> ""),
        "real" -> Map[Token, Object](
          Token.ID -> ""),
    )

  }
}
