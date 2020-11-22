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

        println(tokenWithIntClassification + s" tipinho: ")

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithIntClassification.t_type)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 8 =>
        val tokenWithRealClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        println(tokenWithRealClassification + s" tipinho: ")

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithRealClassification.t_type)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 9 =>
        val tokenWithLiteralClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        println(tokenWithLiteralClassification + s" tipinho: ")

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithLiteralClassification.t_type)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable)

      case 11 =>
        printWriter.write("\tscanf(\" \", );\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case 12 =>
        printWriter.write("\tprintf(\"\");\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case 18 =>
        printWriter.write(s"\tT$sequencer = ;\n")
        SemanticInfo(semanticStack, sequencer + 1, symbolTable)
      case 23 =>
        printWriter.write("\t}\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case 24 =>
        printWriter.write("\tif (){\n")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case 25 =>
        printWriter.write(s"\tT$sequencer = ;\n")
        SemanticInfo(semanticStack, sequencer + 1, symbolTable)
      case 30 =>
        printWriter.write("\n/*fim*/")
        SemanticInfo(semanticStack, sequencer, symbolTable)
      case __ =>
        SemanticInfo(semanticStack, sequencer, symbolTable)
    }
  }
}
