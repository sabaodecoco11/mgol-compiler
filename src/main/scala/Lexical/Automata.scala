package Lexical


import Enum.Token.Token
import Enum.{AutomataAction, CharPattern, Token}



object Automata {
  val wordRangeRangeList = ('a' to 'z').concat('A' to 'Z').toList
  val numberRangeRangeList = ('0' to '9').toList
  val ignoredRangeList = List(' ', '\n', '\t', ';')

  val acceptedStates: Map[Int, Token] = Map[Int, Token](
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

  val automataTransitionTable: Map[Int, Map[String, Int]] = Map(
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
        "=" -> 17
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


}
