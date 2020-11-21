package Lexical

import Enum.CharPattern.CharPattern
import Enum.Token.Token
import Enum.{AutomataAction, CharPattern, D_Type, Token}
import Lexical.Automata.numberRangeRangeList
import Utils.Common.SymbolTable

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object Scanner {

  case class TokenAttribute(lex: String, classification: String, t_type: Option[String])

  case class LexicalProcessing(recognizedToken: TokenAttribute,
                               lastPos: Int,
                               state: Int,
                               line: Int, column: Int, updatedSymbolTable: SymbolTable)

  @tailrec
  final def getToken(str:String,
                     strPos: Int, strSize: Int,
                     line: Int, previousState: Int, lex: String, column: Int, symbolTable: SymbolTable, ignoreComment: Boolean): LexicalProcessing = {
    //caso base
    if(strPos >= strSize )
      return LexicalProcessing(TokenAttribute("EOF", Token.END_OF_FILE, None), -1, -1, -1, -1, symbolTable)

    val ch = str.charAt(strPos)
    val charGroup = getCharGroup(ch)

    val nextState = this.getNextState(charGroup, previousState)

    //para tratativa de erro!
    val newLineCount: Int = if(ch.equals('\n')) line + 1 else line
    val columnCount:Int = if(ch.equals('\n')) 1 else column

    //leu caractere ignorado...
    if(nextState.equals(AutomataAction.INITIAL_STATE))
      getToken(str, strPos + 1, strSize, newLineCount, 0, lex,  columnCount+1, symbolTable, ignoreComment)

    //transição ok
    else if(nextState > AutomataAction.INITIAL_STATE)
      getToken(str, strPos + 1, strSize, line, nextState, lex + ch, columnCount+1, symbolTable, ignoreComment)

    //transitou para caractere que não estava na função de transição
    else if(nextState.equals(AutomataAction.TRANSITION_NOT_FOUND)){
      val acceptanceToken = Automata.acceptedStates(previousState)
      if(!acceptanceToken.isEmpty && !(acceptanceToken.equals(Token.COMMENT) && ignoreComment)) {
          val hasSymbolTableEntry = if(acceptanceToken.equals(Token.ID) && symbolTable.contains(lex)) true else false
          val updatedSymbolTable: SymbolTable =
            if(!hasSymbolTableEntry && acceptanceToken.equals(Token.ID))
              HashMap(lex -> (acceptanceToken, getTypeByState(previousState, lex))) ++ symbolTable
            else
              symbolTable

          val tokenClassification: Token = if(hasSymbolTableEntry) symbolTable(lex)._1 else acceptanceToken
          val candidateToken = TokenAttribute(lex,tokenClassification, Some(getTypeByState(previousState, lex)))

          LexicalProcessing(candidateToken, strPos, 0, line, column, updatedSymbolTable)
      }
      //reconhece o comentário mas não o retorna... segue o baile...
      else if(acceptanceToken.equals(Token.COMMENT) && ignoreComment){
        getToken(str, strPos, strSize, line, 0, "", column, symbolTable, ignoreComment)
      }
      else {
        println("[ERRO LÉXICO] -> Caracterece inesperado: " + {if(ch.equals('\n')) "\\n" else ch.toString} + " linha: " + line + " coluna: " + column )
        LexicalProcessing(TokenAttribute(lex, Token.ERROR, None), strPos, 0, line, columnCount, symbolTable)
      }
    }

    //caractere não existe no alfabeto...
    else{
      println("[ERRO LÉXICO - símbolo inválido] -> Caracterece: " + ch + " linha: " + line + " coluna: " + column )
      LexicalProcessing(TokenAttribute(ch.toString, Token.ERROR, None), strPos+1, 0, line, columnCount, symbolTable)
//      getToken(str, strPos+1, strSize, line, 0, "",  columnCount+1, symbolTable)
    }
  }

  //esta função retorna o estado seguinte do automato, baseado no caractere fornecido
  def getNextState(classification: String, state: Int): Int = {
    Automata.automataTransitionTable(state)(classification)
  }

  private def getCharGroup(c: Char): CharPattern = {
    if(numberRangeRangeList.contains(c))
      CharPattern.NUMERIC

    else if(c.equals('\n'))
      "\n"

    // retorna o próprio caractere
    else
    c.toString
  }

  private def getTypeByState(state: Int, lex: String): String= {
    state match {
      case 5 => D_Type.INTEGER
      case 8 => D_Type.REAL
      case 9 => D_Type.REAL
      case 14 => lex
      case 15 => D_Type.ASSIGN
      case _ => ""
    }
  }
}
