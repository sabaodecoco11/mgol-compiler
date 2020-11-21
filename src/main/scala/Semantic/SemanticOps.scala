package Semantic

import java.io.PrintWriter

import Lexical.Scanner.TokenAttribute
import Syntactic.Productions.NonTerminal


object SemanticOps {
  def action(state: Int, nonTerminal: NonTerminal, semanticStack: List[Any], printWriter: PrintWriter): List[Any] = {
    state match {
      case 5 =>
        printWriter.write("\n\n\n")
        semanticStack

      case 6 => println(semanticStack.head)
        semanticStack

      case 7 =>
        semanticStack
      case 11 =>
        printWriter.write("\tscanf(\" \", );\n")
        semanticStack
      case 12 =>
        printWriter.write("\tprintf(\"\");\n")
        semanticStack
      case 23 =>
        printWriter.write("\t}\n")
        semanticStack
      case 24 =>
        printWriter.write("\tif (){\n")
        semanticStack
      case 30 =>
        printWriter.write("\n/*fim*/")
        semanticStack
      case __ =>
        semanticStack
    }
  }
}
