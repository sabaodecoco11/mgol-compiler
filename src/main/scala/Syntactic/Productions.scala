package Syntactic

import Lexical.Scanner.TokenAttribute

object Productions {
  case class NonTerminal(lhs: String, rhs: String, typeAttr: String, copy: Any)

  val mapper = Map[Int, NonTerminal](
    1 -> NonTerminal("P\'", "P", "", None),
  2 -> NonTerminal("P", "inicio V A", "", None),
  3 -> NonTerminal("V", "varinicio LV", "", None),
  4 -> NonTerminal("LV", "D LV", "", None),
  5 -> NonTerminal("LV", "varfim ;", "", None),
  6 -> NonTerminal("D","id TIPO ;", "", None),
  7 -> NonTerminal("TIPO", "inteiro", "", None),
  8 -> NonTerminal("TIPO","real", "", None),
  9 -> NonTerminal("TIPO","lit", "", None),
  10 -> NonTerminal("A","ES A", "", None),
  11 -> NonTerminal("ES","leia id ;", "", None),
  12 -> NonTerminal("ES","escreva ARG ;", "", None),
  13 -> NonTerminal("ARG","literal", "", None),
  14 -> NonTerminal("ARG","num", "", None),
  15 -> NonTerminal("ARG","id", "", None),
  16 -> NonTerminal("A","CMD A", "", None),
  17 -> NonTerminal("CMD","id rcb LD ;", "", None),
  18 -> NonTerminal("LD","OPRD opm OPRD", "", None),
  19 -> NonTerminal("LD","OPRD", "", None),
  20 -> NonTerminal("OPRD","id", "", None),
  21 -> NonTerminal("OPRD","num", "", None),
  22 -> NonTerminal("A","COND A", "", None),
  23 -> NonTerminal("COND","CABECALHO CORPO", "", None),
  24 -> NonTerminal("CABECALHO","se ( EXP_R ) entao", "", None),
  25 -> NonTerminal("EXP_R","OPRD opr OPRD", "", None),
  26 -> NonTerminal("CORPO","ES CORPO", "", None),
  27 -> NonTerminal("CORPO","CMD CORPO", "", None),
  28 -> NonTerminal("CORPO","COND CORPO", "", None),
  29 -> NonTerminal("CORPO","fimse", "", None),
  30 -> NonTerminal("A","fim", "", None)
  )
}
