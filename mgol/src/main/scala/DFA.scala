
class DFA {
  private val wordRangeRangeList = ('a' to 'z').concat('A' to 'Z').toList
  private val numberRangeRangeList = ('0' to '9').toList
  private val logicalRangeList = ('<' to '>').toList

  val acceptedStates = List(1, 2, 3, 4, 5 , 8, 9, 11, 13, 14, 15, 16, 17, 22, 20, 21);

  private val automataTransitionTable: Map[Int, Map[String, Int]] = Map(
    0 -> Map[String, Int](
        " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0,
        "\\" -> 99,
        "(" -> 1,
        ")" -> 2,
        "letra" -> 4, //qualquer letra
        "digito" -> 5, // qualquer dígito
        "{" -> 10,
        "\"" -> 12,
        "+" -> 14, "-" -> 14, "/" -> 14, "*" -> 14, "-" -> 14,
         ">" -> 16,
        "<" -> 15,
        "=" -> 17,
        "-" -> 20,
        "eof" -> 19
    ).withDefaultValue(-2),

    1 -> Map[String, Int]().withDefaultValue(-1), //estado sem transições; aceitação
    2 -> Map[String, Int]().withDefaultValue(-1), //estado sem transições; aceitação
    3 -> Map[String, Int]().withDefaultValue(-1), //estado sem transições; aceitação

    4 -> Map[String, Int]( //estado de aceitação
      "letra" -> 4, "digito" -> 4, "_" -> 4,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    5 -> Map[String, Int]( //estado de aceitação
      "." -> 6,
      "E" -> 7, "e" -> 7,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    6 -> Map[String, Int](
      "digito"-> 8,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    7 -> Map[String, Int](
      "+" -> 81, "-" -> 81,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    8 -> Map[String, Int](
      "digito" -> 8,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1), //estado sem transições; aceitação

    81 -> Map[String, Int](
      "digito" -> 9,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    9 -> Map[String, Int]( //estado de aceitação
      "digito" -> 9,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    10 -> Map[String, Int](
      "digito" -> 10, "letra" -> 10, "logico" -> 10, "." -> 10, ";" -> 10, "*" -> 10, "+" -> 10,
      "-" -> 10, "/" -> 10, "_" -> 10, "\\" -> 10,
      "}" -> 11,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    11 -> Map[String, Int]().withDefaultValue(-1), //estado sem transições; aceitação

    12 -> Map[String, Int](
      "digito" -> 12,
      "letra" -> 12,
      "logico" -> 12, "." -> 12, ";" -> 12, "*" -> 12, "+" -> 12, " " -> 12, ":" -> 12,
      "-" -> 12, "/" -> 12, "_" -> 12, "\\" -> 12,
      "\"" -> 13,
      "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    13 -> Map[String, Int](
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),//estado sem transições; aceitação

    14 -> Map[String, Int](
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1), //estado sem transições; aceitação

    15 -> Map[String, Int]( //estado de aceitação
      "=" -> 17,
      ">" -> 17,
      "-" -> 20,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    16 -> Map[String, Int]( //estado de aceitação
      "=" -> 17,
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),

    17 -> Map[String, Int](
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1), //estado sem transições; aceitação

    20 -> Map[String, Int](
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1),



    22 -> Map[String, Int](
      " " -> 0, "\n" -> 0, "\t" -> 0, ";" -> 0
    ).withDefaultValue(-1), //estado sem transições; aceitação

    99 -> Map[String, Int](
      "n" -> 0
    ).withDefaultValue(-1)
  )

  private def getCharClassification(c: Char): String = {
    if(wordRangeRangeList.contains(c))
      "letra"
    else if(numberRangeRangeList.contains(c))
      "digito"
    else if(logicalRangeList.contains())
      "logico"

    else if(c.equals('\n'))
      "\n"

    else // retorna o próprio caractére
      c.toString
  }

  private def getCharClassificationByRegex(c:Char): Unit = {
  }

  //esta função retorna o estado seguinte do automato, baseado no caractere fornecido
  def automataProcessing(c: Char, state: Int): Int = {
    val charClassification = this.getCharClassification(c)

    return this.automataTransitionTable(state)(charClassification)
  }

}
