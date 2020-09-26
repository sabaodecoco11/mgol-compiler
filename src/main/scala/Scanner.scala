import DFA.Automata
import Utils.Common.SymbolTable

import scala.annotation.tailrec

object Scanner {

  def begin(lines: Iterator[String], symbolTable: SymbolTable, previousPos: Int): SymbolTable= {

      val line = getStringFromLines(lines, "")

      val symbolT: SymbolTable =
        Automata.processing(line, 0, line.size,1, 0, "", symbolTable, 0)

      return symbolT
  }

  @tailrec
  def getStringFromLines(lines: Iterator[String], str: String): String ={
    if(lines.hasNext){
       getStringFromLines(lines.drop(0), str + lines.next() + "\n" )
    }
    else
      str + "eof"
  }


}
