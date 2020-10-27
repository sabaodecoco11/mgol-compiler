package Syntactic

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


    println("------------------------------------------------------------------")

    val topState = stateStack.head

    println("[estado no topo: " + topState + s"\ttoken no inicio: ${lexData.recognizedToken._2}\n" +
    "configuração da pilha: " + stateStack + "]\n")


    //obtem a ação que deve ser realizada.
    val action = actionTransitionTable(topState.toInt)(lexData.recognizedToken._2)

    //efetua um empilhamento
    if(action.startsWith("S")) {
      val shiftState = action.substring(1)

      println(s"\t>>Realizando shift para para $shiftState<<\n")

      val newlyUpdateLexData = Scanner.getToken(content, lexData.lastPos, content.size, lexData.line,
                                          lexData.state, "", lexData.column,
                                          lexData.updatedSymbolTable, ignoreComment = true)

      println(s"\t>>Novo token obtido:${newlyUpdateLexData.recognizedToken._2} e lex:${newlyUpdateLexData.recognizedToken
      ._1}\n")

      parserProcessing(actionTransitionTable, gotoTransitionTable, shiftState::stateStack, content, newlyUpdateLexData)
    }

    //efetua uma redução
    else if(action.startsWith("R")) {
      val reductionState = action.substring(1);

      //obtêm o estado de redução
      val productionState =  Productions.mapper(reductionState.toInt)

      println(s"\t>>Realizando reduce para $reductionState<<")

      //obtem o número de produções da regra...
      val popNumber = productionState._2.split(" ").size

      println(s"\t\tnumber of following pop's: $popNumber")

      val newStateStack = stateStack.drop(popNumber)

      println(s"\t\tstack after popping: $newStateStack\n")

      val newStateHead = newStateStack.head

      println(s"\tefetuando goto de $newStateHead com a produção ${productionState._1}")

      //obtem o estado que está no topo da pilha
      val gotoState = gotoTransitionTable(newStateHead.toInt)(productionState._1)

      if(gotoState.equals("ERRO")){
        println("Deu ruim demais na hora de configurar o csv!!!!!")
        return;
      }

      parserProcessing(actionTransitionTable, gotoTransitionTable, gotoState::newStateStack, content, lexData)


    }

    else if(action.startsWith("A")){
      println("\n\nAchou aceitacao!!");
      return;
    }

    //rotina de erros.
    else{
      println("Rotina de tratamento de erros...")
      return;
    }


  }

  def init(content: String):Unit = {
    val actionTransitionTable = getAutomataTransition("action.csv")
    val gotoTransitionTable = getAutomataTransition("goto.csv")

    val token = Scanner.getToken(content, 0, content.size, 1, 0, "", 1, Common.getSymbolTable(), true);

    parserProcessing(actionTransitionTable, gotoTransitionTable,
      List[String]("0", "$"),content, token)

  }




}
