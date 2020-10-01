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
    0 -> (Map[String, Int](
        "\t" -> 0, " " -> 0, ";" -> 0, "\n" -> 0,
        "(" -> 1,
        ")" -> 2,
        ";" -> 3,
        CharPattern.NUMERIC -> 5, // qualquer dígito
        "{" -> 10,
        "\"" -> 12,
        "+" -> 14, "-" -> 14, "/" -> 14, "*" -> 14,
         ">" -> 16,
        "<" -> 15,
        "=" -> 17
    ) ++ wordRangeRangeList.map(c => c.toString -> 4).toMap[String, Int])
      .withDefaultValue(AutomataAction.SYMBOL_NOT_FOUND),

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
    4 -> (Map[String, Int]( //estado de aceitação
      CharPattern.NUMERIC -> 4, "_" -> 4
    ) ++ wordRangeRangeList.map(c => c.toString -> 4).toMap[String, Int]).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

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
    10 -> (Map[String, Int](
      CharPattern.NUMERIC -> 10,
      "\n" -> 10,
      CharPattern.LOGICAL -> 10, "." -> 10, "*" -> 10, "+" -> 10, "(" -> 10, ")" -> 10, "+"-> 10, "-"-> 10, "="->10,
      "<"->10,
      "-" -> 10, "/" -> 10, "_" -> 10, "\\" -> 10, " " -> 10, ";" -> 10,  "\"" -> 10, ":" -> 10, "," -> 10,
      "}" -> 11
    ) ++ wordRangeRangeList.map(c => c.toString -> 10).toMap[String, Int])
      .withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

    //state 11
    11 -> Map[String, Int](
    ).withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND), //estado sem transições; aceitação

    //state 12
    12 -> (Map[String, Int](
      CharPattern.NUMERIC -> 12,

      CharPattern.LOGICAL -> 12, "." -> 12, ";" -> 12, "*" -> 12, "+" -> 12, " " -> 12, ";" -> 12,  ":" -> 12,
      "-" -> 12, "/" -> 12, "_" -> 12, "\\" -> 12, "\\" -> 12, "=" -> 12,
      "\"" -> 13
    ) ++ wordRangeRangeList.map(c => c.toString -> 12).toMap[String, Int])
      .withDefaultValue(AutomataAction.TRANSITION_NOT_FOUND),

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
