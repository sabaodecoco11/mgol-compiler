
class DFA {
  private val wordRangeRangeList = ('a' to 'z').concat('A' to 'Z').toList
  private val numberRangeRangeList = ('0' to '9').toList
  private val logicalRangeList = ('<' to '>').toList

  private val automataTransitionTable: Map[Int, Map[String, Int]] = Map(
      0 -> Map[String, Int](
        "(" -> 1,
        ")" -> 2,
        ";" -> 3,
        "letra" -> 4, //qualquer letra
        "digito" -> 5, // qualquer dígito
        "{" -> 6,
        "}" -> -1,  //erro
        "\"" -> 12,
        "+" -> 14, "-" -> 14, "/" -> 14, "*" -> 14,
        "logico" -> 15,
        "-" -> 20,
        "eof" -> 19
    ),

    1 -> Map[String, Int](), //estado sem transições; aceitação
    2 -> Map[String, Int](), //estado sem transições; aceitação
    3 -> Map[String, Int](), //estado sem transições; aceitação

    4 -> Map[String, Int](
      "letra" -> 4, "digito" -> 4, "_" -> 4
    ),

    5 -> Map[String, Int](
      "." -> 6,
      "E" -> 7, "e" -> 7
    ),

    6 -> Map[String, Int](
      "digito"-> 8
    ),

    7 -> Map[String, Int](
      "+" -> 81, "-" -> 81
    ),

    81 -> Map[String, Int](
      "digito" -> 9
    ),

    9 -> Map[String, Int](
      "digito" -> 9
    ),

    10 -> Map[String, Int](
      "digito" -> 10, "letra" -> 10, "logico" -> 10, "." -> 10, ";" -> 10, "*" -> 10, "+" -> 10,
      "-" -> 10, "/" -> 10, "_" -> 10
    ),
    11 -> Map[String, Int](), //estado sem transições; aceitação

    12 -> Map[String, Int](
      "digito" -> 12, "letra" -> 12, "logico" -> 12, "." -> 12, ";" -> 12, "*" -> 12, "+" -> 12,
      "-" -> 12, "/" -> 12, "_" -> 12
    )

  )

  private def getCharClassification(c: Char): String = {
    if(wordRangeRangeList.contains(c))
      "letra"
    else if(numberRangeRangeList.contains(c))
      "digito"
    else if(logicalRangeList.contains())
      "logico"

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