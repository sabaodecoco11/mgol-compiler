import DFA.Automata
import Utils.Common.SymbolTable

import scala.annotation.tailrec

object Scanner {




  @tailrec
  def begin(lines: Iterator[String], linePosition: Int, symbolTable: SymbolTable): SymbolTable= {
    if(lines.hasNext) {

      //é necessário concatenar \n, pois next() a remove da String
      val line = lines.next() + "\n"

      val symbolT: SymbolTable =
        Automata.processing(line, 0, line.size, linePosition, 0, "", symbolTable)

      //chama o processamento da próxima linha
      begin(lines.drop(0), linePosition + 1, symbolTable ++ symbolT);
    }
    else
      symbolTable
  }






}
