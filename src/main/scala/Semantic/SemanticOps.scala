package Semantic

import java.io.PrintWriter

import Enum.Token
import Lexical.Scanner.{LexicalProcessing, TokenAttribute}
import Syntactic.Productions.NonTerminal
import Utils.Common.SymbolTable

import scala.collection.immutable.HashMap


object SemanticOps {
  case class SemanticInfo(semanticStack: List[Any], updatedSequencer: Int, updatedSymbolTable: SymbolTable)

  def action(state: Int, nonTerminal: NonTerminal, terminal: LexicalProcessing,
             semanticStack: List[Any], printWriter: PrintWriter, sequencer: Int, symbolTable: SymbolTable): SemanticInfo = {
    state match {
      case 4 =>
        //neste caso, a declaração das variáveis já foi feita... desempilhe tudo.
        SemanticInfo(semanticStack.empty, sequencer, symbolTable)
      case 5 =>
        printWriter.write("\n\n\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)

      case 6 =>
        val idToken = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] && element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        val nonTerminalInStack = semanticStack.find(element =>
          element.isInstanceOf[NonTerminal])

//        println("this is the nonterminal in stack: " + nonTerminalInStack + '\n' + "and id token: " + idToken)

        //altere o tipo do id
        val updatedSymbolTable = {
          if(idToken.isDefined) {
            val mapEntry = HashMap(idToken.get.asInstanceOf[TokenAttribute].lex -> Tuple2(Token.ID, nonTerminalInStack.get.asInstanceOf[NonTerminal].typeAttr))
            val tempRemovedEntry = symbolTable.removed(idToken.get.asInstanceOf[TokenAttribute].lex)
            tempRemovedEntry++mapEntry
          }
          else symbolTable
        }

        printWriter.write(s"\t${nonTerminalInStack.get.asInstanceOf[NonTerminal].typeAttr} ${idToken.get.asInstanceOf[TokenAttribute].lex};\n")

        SemanticInfo(semanticStack.empty, sequencer, updatedSymbolTable)

      case 7 =>
        //obtenho o token id.
        val tokenWithIntClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithIntClassification.t_type, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 8 =>
        val tokenWithRealClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithRealClassification.t_type, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 9 =>
        val tokenWithLiteralClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithLiteralClassification.t_type, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 11 =>
        val tokenWithIdClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] &&
            element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        if(tokenWithIdClassification.isDefined && !tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type.isEmpty){
          val tknType = tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type
          val tknLex = tokenWithIdClassification.get.asInstanceOf[TokenAttribute].lex
          tknType match {
            case "int" => printWriter.write("\tscanf(\"" + "%d\", " + tknLex + ");\n")
            case "real" => printWriter.write("\tscanf(\"" + "%lf\", " + tknLex + ");\n")
            case "literal" => printWriter.write("\tscanf(\"" + "%s\", " + tknLex + ");\n")
          }
          SemanticInfo(semanticStack.empty, sequencer, symbolTable)
        }
        else{

          return null
        }


      case 12 =>
        val nonTerminalArg = semanticStack.find(element => element.isInstanceOf[NonTerminal])

        printWriter.write(s"\tprintf(${nonTerminalArg.get.asInstanceOf[NonTerminal].typeAttr});\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case 13 =>
        val tokenWithLiteralClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithLiteralClassification.lex, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 18 =>
        printWriter.write(s"\tT$sequencer = ;\n")
        SemanticInfo(semanticStack, sequencer + 1, symbolTable)

      case 20 =>
        val tokenWithIdClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] &&
            element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        if(tokenWithIdClassification.isDefined && !tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type.isEmpty){
          println("Instancia ok!")

          val updatedSemanticStack =
            NonTerminal(nonTerminal.lhs, nonTerminal.rhs, nonTerminal.typeAttr, tokenWithIdClassification.get.asInstanceOf[TokenAttribute])::semanticStack

          SemanticInfo(updatedSemanticStack, sequencer, symbolTable)
        }
        else
          //todo LANÇAR EXCEÇÃO
          return null

      case 21 =>
        val tokenWithNumClassification = semanticStack.head

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, nonTerminal.typeAttr, tokenWithNumClassification.asInstanceOf[TokenAttribute])::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 23 =>
        printWriter.write("\t}\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case 24 =>
        printWriter.write("\tif (){\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case 25 =>
        //obtém os operadores na ordem de inserção
        val tokensWithOPRDClassification = semanticStack.filter(element =>
          element.isInstanceOf[NonTerminal]).reverse

        val tokenWithOPRClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] && element.asInstanceOf[TokenAttribute].classification.equals(Token.REL_OPERATOR))

        val types = tokensWithOPRDClassification.map(t => t.asInstanceOf[NonTerminal].copy.asInstanceOf[TokenAttribute].t_type)

        val typeResultComparison: Boolean = types.forall(element => element.equals(types.head))

        println(s"SIZE: " + tokensWithOPRDClassification.size + " > " + tokensWithOPRDClassification + "\nresult: " + typeResultComparison)

        val temporaryExpression = tokenWithOPRClassification.get.asInstanceOf[TokenAttribute].lex

        printWriter.write(s"\tT$sequencer = ${temporaryExpression}\n")
        SemanticInfo(semanticStack, sequencer + 1, symbolTable)
      case 30 =>
        printWriter.write("\n/*fim*/")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case __ =>
        SemanticInfo(semanticStack, sequencer, symbolTable)
    }
  }
}
