import DFA.Automata
import Enum.CharPattern
import Enum.CharPattern.CharPattern

import scala.annotation.tailrec

object Scanner {




  @tailrec
  def begin(lines: Iterator[String], linePosition: Int): Unit= {
    if(lines.hasNext){

      //é necessário concatenar \n, pois next() a remove da String
      val line = lines.next() + "\n"

      Automata.processing(line, 0, line.size, linePosition, 0, "")

      //chama o processamento da próxima linha
      begin(lines.drop(0), linePosition + 1);
    }
  }






}
