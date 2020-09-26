package DFA


import Enum.CharPattern.CharPattern
import Enum.Token.Token
import Enum.{AutomataAction, CharPattern, D_Type, Token}
import Utils.Common.SymbolTable

import scala.annotation.tailrec
import scala.collection.immutable.HashMap


object Automata {
  private val wordRangeRangeList = ('a' to 'z').concat('A' to 'Z').toList
  private val numberRangeRangeList = ('0' to '9').toList
  private val ignoredRangeList = List(' ', '\n', '\t', ';')

  private val acceptedStates: Map[Int, Token] = Map[Int, Token](
    1 -> Token.OP_PARENTHESIS,
    2 -> Token.CL_PARENTHESIS, 3 -> Token.SEMICOLON,
    4 -> Token.ID, 5 -> Token.NUMBER ,
    8 -> Token.NUMBER,
    9 -> Token.NUMBER, 11 -> Token.COMMENT, 13 -> Token.LITERAL, 14 -> Token.MATH_OPERATOR,
    15 -> Token.REL_OPERATOR, 16 -> Token.REL_OPERATOR, 17 -> Token.REL_OPERATOR,
    20 -> Token.ATTR,
    22 -> Token.END_OF_FILE).withDefaultValue("")

  val rejectionStates: Map[Int, String] = Map[Int, Token](
    6 -> Token.NUMBER,
    7 -> Token.NUMBER,
    81 -> Token.NUMBER,
    10 -> Token.COMMENT, 12-> Token.LITERAL)

  private val automataTransitionTable: Map[Int, Map[String, Int]] = Map(
    //state 0
    0 -> Map[String, Int](
        "\t" -> 0, " " -> 0, ";" -> 0, "\n" -> 0,
        "(" -> 1,
        ")" -> 2,
        ";" -> 3,

        "a" -> 4, "b" -> 4, "c" -> 4, "d" -> 4, "e" -> 4, "f" -> 4, "g" -> 4, "h" -> 4, "i" -> 4, "j" -> 4, "k" -> 4, //qualquer letra
        "l" -> 4, "m" -> 4, "n" -> 4, "o" -> 4, "p" -> 4, "q" -> 4, "r" -> 4, "s" -> 4, "t"->4, "u" -> 4, "v" -> 4,
        "w" -> 4, "x" -> 4, "y" -> 4, "z" -> 4,

        CharPattern.NUMERIC -> 5, // qualquer dígito
        "{" -> 10,
        "\"" -> 12,
        "+" -> 14, "-" -> 14, "/" -> 14, "*" -> 14,
         ">" -> 16,
        "<" -> 15,
        "=" -> 17,
        "eof" -> 22
    ).withDefaultValue(AutomataAction.SYMBOL_NOT_FOUND),

    //state 1
    1 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 2
    2 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 3
    3 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 4
    4 -> Map[String, Int]( //estado de aceitação
      "a" -> 4, "b" -> 4, "c" -> 4, "d" -> 4, "e" -> 4, "f" -> 4, "g" -> 4, "h" -> 4, "i" -> 4, "j" -> 4, "k" -> 4, //qualquer letra
      "l" -> 4, "m" -> 4, "n" -> 4, "o" -> 4, "p" -> 4, "q" -> 4, "r" -> 4, "s" -> 4, "t"->4, "u" -> 4, "v" -> 4,
      "w" -> 4, "x" -> 4, "y" -> 4, "z" -> 4,
      CharPattern.NUMERIC -> 4, "_" -> 4
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 5
    5 -> Map[String, Int]( //estado de aceitação
      CharPattern.NUMERIC -> 5,
      "." -> 6,
      "E" -> 7, "e" -> 7
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 6
    6 -> Map[String, Int](
      CharPattern.NUMERIC-> 8
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 7
    7 -> Map[String, Int](
      "+" -> 81, "-" -> 81,
      CharPattern.NUMERIC -> 9
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 8
    8 -> Map[String, Int](
      CharPattern.NUMERIC -> 8,
      "E" -> 7, "e" -> 7
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 81
    81 -> Map[String, Int](
      CharPattern.NUMERIC -> 9
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 9
    9 -> Map[String, Int]( //estado de aceitação
      CharPattern.NUMERIC -> 9
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 10
    10 -> Map[String, Int](
      CharPattern.NUMERIC -> 10,

      "a" -> 10, "b" -> 10, "c" -> 10, "d" -> 10, "e" -> 10, "f" -> 10, "g" -> 10, "h" -> 10, "i" -> 10, "j" -> 10, "k" -> 10, //qualquer letra
      "l" -> 10, "m" -> 10, "n" -> 10, "o" -> 10, "p" -> 10, "q" -> 10, "r" -> 10, "s" -> 10, "t"-> 10, "u" -> 10, "v" -> 10,
      "w" -> 10, "x" -> 10, "y" -> 10, "z" -> 10, "\n" -> 10,

      CharPattern.LOGICAL -> 10, "." -> 10, "*" -> 10, "+" -> 10, "(" -> 10, ")" -> 10, "+"-> 10, "-"-> 10, "="->10,
      "<"->10,
      "-" -> 10, "/" -> 10, "_" -> 10, "\\" -> 10, " " -> 10, ";" -> 10,  "\"" -> 10, ":" -> 10, "," -> 10,
      "}" -> 11
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 11
    11 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 12
    12 -> Map[String, Int](
      CharPattern.NUMERIC -> 12,

      "a" -> 12, "b" -> 12, "c" -> 12, "d" -> 12, "e" -> 12, "f" -> 12, "g" -> 12, "h" -> 12, "i" -> 12, "j" -> 12, "k" -> 12, //qualquer letra
      "l" -> 12, "m" -> 12, "n" -> 12, "o" -> 12, "p" -> 12, "q" -> 12, "r" -> 12, "s" -> 12, "t"->12, "u" -> 12, "v" -> 12,
      "w" -> 12, "x" -> 12, "y" -> 12, "z" -> 12,

      CharPattern.LOGICAL -> 12, "." -> 12, ";" -> 12, "*" -> 12, "+" -> 12, " " -> 12, ";" -> 12,  ":" -> 12,
      "-" -> 12, "/" -> 12, "_" -> 12, "\\" -> 12, "\\" -> 12, "=" -> 12,
      "\"" -> 13
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 13
    13 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),//estado sem transições; aceitação

    //state 14
    14 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 15
    15 -> Map[String, Int]( //estado de aceitação
      "=" -> 17,
      ">" -> 17,
      "-" -> 20
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 16
    16 -> Map[String, Int]( //estado de aceitação
      "=" -> 17
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 17
    17 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 20
    20 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 22
    22 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

  )


  //esta função retorna o estado seguinte do automato, baseado no caractere fornecido
  def getNextState(classification: String, state: Int): Int = {
     this.automataTransitionTable(state)(classification)
  }

  @tailrec
  final def processing(str:String,
                       strPos: Int, strSize: Int,
                       line: Int, previousState: Int, lex: String, column: Int, symbolTable: SymbolTable): Tuple5[SymbolTable, Int, Int, Int, Int] = {
    //caso base
    if(strPos >= strSize ) {
      return (HashMap("EOF" -> (Token.END_OF_FILE, None)), -1, -1, -1, -1)
    }

    val ch = str.charAt(strPos)
    val charGroup = getCharGroup(ch)

    val nextState = this.getNextState(charGroup, previousState)

    //para tratativa de erro!
    val newLineCount: Int = if(ch.equals('\n')) line + 1 else line
    val columnCount:Int = if(ch.equals('\n')) 0 else column

    //leu caractere ignorado...
    if(nextState.equals(AutomataAction.INITIAL_STATE))
        processing(str, strPos + 1, strSize, newLineCount, 0, lex,  columnCount+1, symbolTable)

    //transição ok
    else if(nextState > AutomataAction.INITIAL_STATE)
      processing(str, strPos + 1, strSize, newLineCount, nextState, lex + ch, columnCount+1, symbolTable)

    //transitou para caractere que não estava na função de transição
    else if(nextState.equals(AutomataAction.TRANSITION_NOT_FOUND)){
      val acceptanceToken = acceptedStates(previousState)
      if(!acceptanceToken.isEmpty) {
        if(acceptanceToken.equals(Token.ID) && symbolTable.contains(lex))
          (HashMap(lex -> (symbolTable(lex)._1, symbolTable(lex)._2)), strPos, 0, line, columnCount)
        else
          (HashMap(lex -> (acceptanceToken, getTypeByState(previousState))), strPos, 0, line, columnCount)
      }
      else
        (HashMap(lex -> (Token.ERROR, None)),strPos, 0, line, columnCount)
    }

    //caractere não existe no alfabeto...
    else{
      println("[ERRO de símbolo inválido] -> Caracterece: " + ch + " linha: " + line + " coluna: " + column )
      processing(str, strPos+1, strSize, line, 0, "",  columnCount+1, symbolTable)
    }
  }


  private def getCharGroup(c: Char): CharPattern = {
    if(wordRangeRangeList.contains(c))
      c.toLower.toString
    else if(numberRangeRangeList.contains(c))
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
