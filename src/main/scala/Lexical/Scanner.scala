package Lexical

import Enum.CharPattern.CharPattern
import Enum.{AutomataAction, CharPattern, D_Type, Token}
import Lexical.Automata.numberRangeRangeList
import Utils.Common.SymbolTable

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object Scanner {

  case class LexicalProcessing(symbTableEntry: SymbolTable, lastPos: Int, state: Int, line: Int, column: Int )

  @tailrec
  final def getToken(str:String,
                     strPos: Int, strSize: Int,
                     line: Int, previousState: Int, lex: String, column: Int, symbolTable: SymbolTable): LexicalProcessing = {
    //caso base
    if(strPos >= strSize )
      return LexicalProcessing(HashMap("EOF" -> (Token.END_OF_FILE, None)), -1, -1, -1, -1)

    val ch = str.charAt(strPos)
    val charGroup = getCharGroup(ch)

    val nextState = this.getNextState(charGroup, previousState)

    //para tratativa de erro!
    val newLineCount: Int = if(ch.equals('\n')) line + 1 else line
    val columnCount:Int = if(ch.equals('\n')) 0 else column

    //leu caractere ignorado...
    if(nextState.equals(AutomataAction.INITIAL_STATE))
      getToken(str, strPos + 1, strSize, newLineCount, 0, lex,  columnCount+1, symbolTable)

    //transição ok
    else if(nextState > AutomataAction.INITIAL_STATE)
      getToken(str, strPos + 1, strSize, newLineCount, nextState, lex + ch, columnCount+1, symbolTable)

    //transitou para caractere que não estava na função de transição
    else if(nextState.equals(AutomataAction.TRANSITION_NOT_FOUND)){
      val acceptanceToken = Automata.acceptedStates(previousState)
      if(!acceptanceToken.isEmpty) {
        if(acceptanceToken.equals(Token.ID) && symbolTable.contains(lex))
          LexicalProcessing(HashMap(lex -> (symbolTable(lex)._1, symbolTable(lex)._2)), strPos, 0, line, columnCount)
        else
          LexicalProcessing(HashMap(lex -> (acceptanceToken, getTypeByState(previousState))), strPos, 0, line, columnCount)
      }
      else {
        println("[ERRO de fechamento] -> Caracterece inesperado: " + {if(ch.equals('\n')) "\\n" else ch.toString} + " linha: " + line + " coluna: " + column )
        LexicalProcessing(HashMap(lex -> (Token.ERROR, None)),strPos, 0, line, columnCount)
      }
    }

    //caractere não existe no alfabeto...
    else{
      println("[ERRO de símbolo inválido] -> Caracterece: " + ch + " linha: " + line + " coluna: " + column )
      LexicalProcessing(HashMap(ch.toString -> (Token.ERROR, None)), strPos+1, 0, line, columnCount)
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

  private def getTypeByState(state: Int): String= {
    state match {
      case 5 => D_Type.INTEGER
      case 8 => D_Type.REAL
      case 9 => D_Type.REAL
      case _ => ""
    }
  }
}
