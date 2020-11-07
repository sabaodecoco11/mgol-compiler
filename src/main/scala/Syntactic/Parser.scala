package Syntactic

import Enum.Token
import Lexical.Scanner
import Lexical.Scanner.LexicalProcessing
import Utils.{CSV_Processor, Common}

import scala.annotation.tailrec
import scala.io.Source

object Parser {
  type Transition = Map[Int, Map[String, String]]

  case class SyntaxSetupCheck(token: LexicalProcessing, panicModeEnabled: Boolean)

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

  def init(content: String):Unit = {
    val actionTransitionTable = getAutomataTransition("action.csv")
    val gotoTransitionTable = getAutomataTransition("goto.csv")

    val token = Scanner.getToken(content, 0, content.size, 1, 0, "", 1, Common.getSymbolTable(), true);

    parserProcessing(actionTransitionTable, gotoTransitionTable,
      List[String]("0"),content, token, None)

  }

  @tailrec
  private def parserProcessing(actionTransitionTable: Transition, gotoTransitionTable: Transition,
                               stateStack: List[String],
                               content: String, lexData: LexicalProcessing,
                               panicSet: Option[Set[String]]): Unit = {


//    println("------------------------------------------------------------------")

    val topState = stateStack.head

    //se não está no modo pânico, obtêm o token que está na pilha e o modo pânico falso,
    //senão, obtêm um novo token e informação se ele é sincronizador
    val syntaxSetupCheck: SyntaxSetupCheck = {
      if(panicSet.isEmpty){
        SyntaxSetupCheck(lexData, panicModeEnabled = false)
      }
      else{

        //requisita novo token
        val updatedTokenData = Scanner.getToken(content, lexData.lastPos, content.size, lexData.line,
          lexData.state, "", lexData.column,
          lexData.updatedSymbolTable, ignoreComment = true)

//        println(s"Tipo do novo token: ${updatedTokenData.recognizedToken._2} ")
        if(panicSet.get.contains(updatedTokenData.recognizedToken._2)){
          println("Sincronizando análise sintática.")
        }
        SyntaxSetupCheck(updatedTokenData, !panicSet.get.contains(updatedTokenData.recognizedToken._2))
      }
    }

    //enquanto o modo panico estiver ligado, significa que o token sincronizador não foi encontrado... prosiga!
    if(syntaxSetupCheck.panicModeEnabled){
      if(syntaxSetupCheck.token.recognizedToken._2.equals(Token.END_OF_FILE)){
        println("Não foi possível recuperar do modo pânico... sinto muito.")
        return;
      }

      println("Continuando modo pânico...")
      parserProcessing(actionTransitionTable, gotoTransitionTable, stateStack, content, syntaxSetupCheck.token, panicSet)
    }

    else{

      val syntaxLexicalData = syntaxSetupCheck.token

//      println("[estado no topo: " + topState + s"\ttoken no inicio: ${syntaxLexicalData.recognizedToken._2}\n" +
//        "configuração da pilha: " + stateStack + "]\n")


      //obtem a ação que deve ser realizada.
      val action = actionTransitionTable(topState.toInt)(syntaxLexicalData.recognizedToken._2)

      //efetua um empilhamento
      if(action.startsWith("S")) {
        val shiftState = action.substring(1)

//        println(s"\t>>Realizando shift para para $shiftState<<\n")

        val updatedTokenData = Scanner.getToken(content, syntaxLexicalData.lastPos, content.size, syntaxLexicalData.line,
                                            syntaxLexicalData.state, "", syntaxLexicalData.column,
                                            syntaxLexicalData.updatedSymbolTable, ignoreComment = true)

//        println(s"\t>>Novo token obtido:${updatedTokenData.recognizedToken._2} e lex:${updatedTokenData.recognizedToken
//        ._1}\n")

        parserProcessing(actionTransitionTable, gotoTransitionTable, shiftState::stateStack, content, updatedTokenData, None)
      }

      //efetua uma redução
      else if(action.startsWith("R")) {
        val reductionState = action.substring(1);

        //obtêm o estado de redução
        val productionState =  Productions.mapper(reductionState.toInt)

//        println(s"\t>>Realizando reduce para $reductionState<<")

        //obtem o número de produções da regra...
        val popNumber = productionState._2.split(" ").size

//        println(s"\t\tnumber of following pop's: $popNumber")

        val newStateStack = stateStack.drop(popNumber)

//        println(s"\t\tstack after popping: $newStateStack\n")

        val newStateHead = newStateStack.head

//        println(s"\tefetuando goto de $newStateHead com a produção ${productionState._1}")

        //obtem o estado que está no topo da pilha
        val gotoState = gotoTransitionTable(newStateHead.toInt)(productionState._1)

        if(gotoState.equals("ERRO")){
          println("Deu ruim demais na hora de configurar o csv!!!!!")
          return;
        }

        parserProcessing(actionTransitionTable, gotoTransitionTable, gotoState::newStateStack, content, syntaxLexicalData, None)


      }

      else if(action.startsWith("A")){
        println("\n\nAchou aceitacao!!");
        return;
      }

      //rotina de erros.
      else{
        val expectedTokens = actionTransitionTable(topState.toInt).keySet

        println(s"[ERRO SINTÁTICO] na linha ${lexData.line} e coluna ${lexData.column}..." +
                s" Esperava um desses tokens: ${expectedTokens.mkString(", ")}")
        println("[[Iniciando modo pânico!...]]")
        parserProcessing(actionTransitionTable, gotoTransitionTable, stateStack, content, syntaxLexicalData, Some(expectedTokens))
      }
    }

  }






}
