package Syntactic

import java.io.PrintWriter

import Enum.Token
import Lexical.Scanner
import Lexical.Scanner.LexicalProcessing
import Semantic.SemanticOps
import Syntactic.Productions.NonTerminal
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

    val token = Scanner.getToken(content, 0, content.size, line = 1,
      previousState = 0, lex = "", column = 1, Common.getSymbolTable(), ignoreComment = true);

    val printWriter = new PrintWriter("/home/sabaodecoco/mgol-compiler/alg.c")

    printWriter.write("#include<stdio.h>\n")
    printWriter.write("typedef char literal[256];\n")
    printWriter.write("typedef double real;\n\n")
    printWriter.write("typedef int inteiro;\n\n")
    printWriter.write("void main(void){\n")


    parserProcessing(actionTransitionTable, gotoTransitionTable,
      List[String]("0"), List[Any](),content, token, None, sequencer = 0, printWriter)

    printWriter.close();

  }

  @tailrec
  private def parserProcessing(actionTransitionTable: Transition, gotoTransitionTable: Transition,
                               stateStack: List[String], semanticStack: List[Any],
                               content: String, lexData: LexicalProcessing,
                               panicSet: Option[Set[String]], sequencer: Int, printWriter: PrintWriter): Unit = {

    println("------------------------------------------------------------------")

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

        println(s"Tipo do novo token: ${updatedTokenData.recognizedToken.classification} ")
        if(panicSet.get.contains(updatedTokenData.recognizedToken.classification)){
          println("Sincronizando análise sintática.")
        }
        SyntaxSetupCheck(updatedTokenData, !panicSet.get.contains(updatedTokenData.recognizedToken.classification))
      }
    }

    //enquanto o modo panico estiver ligado, significa que o token sincronizador não foi encontrado... prosiga!
    if(syntaxSetupCheck.panicModeEnabled){
      if(syntaxSetupCheck.token.recognizedToken.classification.equals(Token.END_OF_FILE)){
        println("Não foi possível recuperar do modo pânico... sinto muito.")
        return;
      }

      println("Continuando modo pânico...")
      parserProcessing(actionTransitionTable, gotoTransitionTable,
        stateStack, semanticStack, content, syntaxSetupCheck.token, panicSet, sequencer, printWriter)
    }

    else{

      val syntaxLexicalData = syntaxSetupCheck.token

      println("[estado no topo: " + topState + s"\ttoken no inicio: ${syntaxLexicalData.recognizedToken.classification}" +
        " configuração da pilha: " + stateStack + "]"
        + s"\n[configuração da pilha semantica: $semanticStack]\n")


      //obtem a ação que deve ser realizada.
      val action = actionTransitionTable(topState.toInt)(syntaxLexicalData.recognizedToken.classification)

      //efetua um empilhamento
      if(action.startsWith("S")) {
        val shiftState = action.substring(1)

        println(s"\t>>Realizando shift para para $shiftState<<\n")

        val updatedTokenData = Scanner.getToken(content, syntaxLexicalData.lastPos, content.size, syntaxLexicalData.line,
                                            syntaxLexicalData.state, "", syntaxLexicalData.column,
                                            syntaxLexicalData.updatedSymbolTable, ignoreComment = true)

        println(s"\t>>Novo token obtido:${updatedTokenData.recognizedToken.classification} e lex:${updatedTokenData.recognizedToken
        .lex}\n")

        parserProcessing(actionTransitionTable, gotoTransitionTable,
          shiftState::stateStack, syntaxLexicalData.recognizedToken::semanticStack, content, updatedTokenData, None, sequencer, printWriter)
      }

      //efetua uma redução
      else if(action.startsWith("R")) {
        val productionNumber = action.substring(1);

        //obtêm o estado de redução
        val nonTerminal =  Productions.mapper(productionNumber.toInt)

        println(s"\t>>Realizando reduce para $productionNumber<<")

        //realiza verificação semântica
        val newSemanticStack = SemanticOps.action(productionNumber.toInt, nonTerminal, semanticStack, printWriter)

        //obtem o número de produções da regra...
        val popNumber = nonTerminal.rhs.split(" ").size

        println(s"\t\tnumber of following pop's: $popNumber")

        val newStateStack = stateStack.drop(popNumber)

        println(s"\t\tstack after popping: $newStateStack\n")

        val newStateHead = newStateStack.head

        println(s"\tefetuando goto de $newStateHead com a produção ${nonTerminal.lhs}")

        //obtem o estado que está no topo da pilha
        val gotoState = gotoTransitionTable(newStateHead.toInt)(nonTerminal.lhs)

        if(gotoState.equals("ERRO")){
          println("Deu ruim demais na hora de configurar o csv!!!!!")
          return;
        }

        parserProcessing(actionTransitionTable, gotoTransitionTable,
          gotoState::newStateStack, newSemanticStack, content, syntaxLexicalData, None, sequencer, printWriter)

      }

      else if(action.startsWith("A")){
        println("\n\nAchou aceitacao!!");
      }

      //rotina de erros (modo pânico).
      else{
        val expectedTokens = actionTransitionTable(topState.toInt).keySet

        println(s"[ERRO SINTÁTICO] na linha ${lexData.line} e coluna ${lexData.column}..." +
                s" Esperava um desses tokens: ${expectedTokens.mkString(", ")}")
        println("[[Iniciando modo pânico!...]]")
        parserProcessing(actionTransitionTable, gotoTransitionTable,
          stateStack, semanticStack, content, syntaxLexicalData, Some(expectedTokens), sequencer, printWriter)
      }
    }

  }






}
