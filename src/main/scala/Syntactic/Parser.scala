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
    val state = stateStack.last

    if(state.equals("$")) println("Syntax analysis done!")

    //request token
    val updatedLexData =
      Scanner.getToken(content, lexData.lastPos, content.size, lexData.line, lexData.state, "", lexData.column,
                                                                    lexData.updatedSymbolTable, ignoreComment = true)

    if(updatedLexData.recognizedToken.contains("EOF")) return;


    //mostra o lexema, token e tipo
    for((k,v) <- updatedLexData.recognizedToken)
      println(k.replace("\n", " ") + " " + v._1 + " " + v._2)

    parserProcessing(actionTransitionTable, gotoTransitionTable, stateStack, content, updatedLexData)


  }

  def init(content: String):Unit = {
    val actionTransitionTable = getAutomataTransition("action.csv")
    val gotoTransitionTable = getAutomataTransition("goto.csv")


    parserProcessing(actionTransitionTable, gotoTransitionTable,
      List[String]("$", "0"),content, LexicalProcessing(null, 0, 1, 1, 0, Common.getSymbolTable()))

  }




}
