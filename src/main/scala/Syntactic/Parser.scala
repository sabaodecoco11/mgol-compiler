package Syntactic

import Enum.Token
import Lexical.Scanner
import Lexical.Scanner.LexicalProcessing
import Utils.{CSV_Processor, Common}

import scala.annotation.tailrec
import scala.io.Source

object Parser {
  type Transition = Map[Int, Map[String, String]]

  private def getAutomataTransition(pathToFile: String):Transition = {
    try{
      val fileSource = Source.fromResource(pathToFile)

      val strList = fileSource.getLines().toList

      fileSource.close()

      CSV_Processor.getTransitionTable(0, strList.drop(1), strList.head, Map.empty)
    }catch{
      case e: Exception => Map.empty
    }

  }

  @tailrec
  private def parserProcessing(actionTransitionTable: Transition,
                               gotoTransitionTable: Transition,
                               stateStack: List[String],
                               content: String,
                              lexData: LexicalProcessing): Unit = {
    val state = stateStack.head

//    println("[estado no topo: " + state + "]")

    if(lexData.recognizedToken._2.equals("EOF")) return;

    //mostra o lexema, token e tipo
//      println(lexData.recognizedToken._1 + " " + lexData.recognizedToken._2 + " " + lexData.recognizedToken._3)

    val action = actionTransitionTable(state.toInt)(lexData.recognizedToken._2)

    if(action.startsWith("S")) {
      println(action)
      val newlyUpdateLexData = Scanner.getToken(content, lexData.lastPos, content.size, lexData.line,
                                          lexData.state, "", lexData.column,
                                          lexData.updatedSymbolTable, ignoreComment = true)

      parserProcessing(actionTransitionTable, gotoTransitionTable, action.substring(1)::stateStack, content, newlyUpdateLexData)
    }
    else if(action.startsWith("R")) {
      println("...reduzindo: ")
      val production =  Productions.mapper(action.substring(1).toInt)

      //obtem o número de produções da regra...
      val popNumber = production._2.split(" ").size

      println("\tnumber of following pop's: " + popNumber)

      val newStateStack = stateStack.drop(popNumber)

      //obtem o estado que está no topo da pilha
      val gotoState = gotoTransitionTable(newStateStack.head.toInt)(production._1)

      parserProcessing(actionTransitionTable, gotoTransitionTable, gotoState::newStateStack, content, lexData)


    }

    else if(action.startsWith("A")){
      println("\n\nAchou aceitacao!!");
      parserProcessing(actionTransitionTable, gotoTransitionTable, action.charAt(1).toString::stateStack, content, lexData)

    }
    else{println("Rotina de tratamento de erros...")
      return;
    }


  }

  def init(content: String):Unit = {
    val actionTransitionTable = getAutomataTransition("action.csv")
    val gotoTransitionTable = getAutomataTransition("goto.csv")


    parserProcessing(actionTransitionTable, gotoTransitionTable,
      List[String]("0", "$"),content, Scanner.getToken(content, 0, content.size, 1, 0, "", 1, Common.getSymbolTable(), true))

  }




}
