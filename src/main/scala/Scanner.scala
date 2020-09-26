import DFA.Automata
import Utils.Common.SymbolTable

import scala.annotation.tailrec

object Scanner {

  @tailrec
  def begin(content: String, strPos: Int, strSize: Int, line: Int, column: Int,  previousState: Int, lexeme: String, symbolTable: SymbolTable ): Unit= {

      val lexemeInfo: Tuple5[SymbolTable, Int, Int, Int, Int] =
        Automata.processing(content, strPos, content.size, line, previousState, "", column, symbolTable)

      //passo base
      if(lexemeInfo._1.contains("EOF")){
        println("EOF")
        return;
      }

      else{
        println(lexemeInfo._1)
        begin(content, lexemeInfo._2, content.size, lexemeInfo._4, lexemeInfo._5, lexemeInfo._3, "", symbolTable)
      }


  }

}
