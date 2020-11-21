package Syntactic

object Productions {
  case class NonTerminal(lhs: String, rhs: String, typeAttr: Any)

  val mapper = Map[Int, NonTerminal](
    1 -> NonTerminal("P\'", "P", ""),
  2 -> NonTerminal("P", "inicio V A", ""),
  3 -> NonTerminal("V", "varinicio LV", ""),
  4 -> NonTerminal("LV", "D LV", ""),
  5 -> NonTerminal("LV", "varfim ;", ""),
  6 -> NonTerminal("D","id TIPO ;", ""),
  7 -> NonTerminal("TIPO", "inteiro", ""),
  8 -> NonTerminal("TIPO","real", ""),
  9 -> NonTerminal("TIPO","lit", ""),
  10 -> NonTerminal("A","ES A", ""),
  11 -> NonTerminal("ES","leia id ;", ""),
  12 -> NonTerminal("ES","escreva ARG ;", ""),
  13 -> NonTerminal("ARG","literal", ""),
  14 -> NonTerminal("ARG","num", ""),
  15 -> NonTerminal("ARG","id", ""),
  16 -> NonTerminal("A","CMD A", ""),
  17 -> NonTerminal("CMD","id rcb LD ;", ""),
  18 -> NonTerminal("LD","OPRD opm OPRD", ""),
  19 -> NonTerminal("LD","OPRD", ""),
  20 -> NonTerminal("OPRD","id", ""),
  21 -> NonTerminal("OPRD","num", ""),
  22 -> NonTerminal("A","COND A", ""),
  23 -> NonTerminal("COND","CABECALHO CORPO", ""),
  24 -> NonTerminal("CABECALHO","se ( EXP_R ) entao", ""),
  25 -> NonTerminal("EXP_R","OPRD opr OPRD", ""),
  26 -> NonTerminal("CORPO","ES CORPO", ""),
  27 -> NonTerminal("CORPO","CMD CORPO", ""),
  28 -> NonTerminal("CORPO","COND CORPO", ""),
  29 -> NonTerminal("CORPO","fimse", ""),
  30 -> NonTerminal("A","fim", ""),
  )
}
