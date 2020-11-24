package Semantic

import java.io.PrintWriter

import Enum.Token
import Lexical.Scanner.{LexicalProcessing, TokenAttribute}
import Syntactic.Productions.NonTerminal
import Utils.Common.SymbolTable

import scala.collection.immutable.HashMap


object SemanticOps {
  case class SemanticInfo(semanticStack: List[Any], updatedSequencer: Int, updatedSymbolTable: SymbolTable, content:String, tempContent: String)

  def action(state: Int, nonTerminal: NonTerminal, terminal: LexicalProcessing,
             semanticStack: List[Any], sequencer: Int, symbolTable: SymbolTable, content:String, tempContent: String): SemanticInfo = {
    state match {
      case 2 =>
        val insertedContent = "\n}\n"
        SemanticInfo(semanticStack.empty, sequencer, symbolTable, content.concat(insertedContent), tempContent)
      case 4 =>
        //neste caso, a declaração das variáveis já foi feita... desempilhe tudo.
        SemanticInfo(semanticStack.empty, sequencer, symbolTable, content, tempContent)
      case 5 =>
        val insertedContent = "\n\n\n"
        SemanticInfo(semanticStack, sequencer, symbolTable, content + insertedContent, tempContent)

      case 6 =>
        val idToken = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] && element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        val nonTerminalInStack = semanticStack.find(element =>
          element.isInstanceOf[NonTerminal])

//        println("this is the nonterminal in stack: " + nonTerminalInStack + '\n' + "and id token: " + idToken)

        //altere o tipo do id
        val updatedSymbolTable = {
          if(idToken.isDefined) {
            val mapEntry = HashMap(idToken.get.asInstanceOf[TokenAttribute].lex -> Tuple2(Token.ID, nonTerminalInStack.get.asInstanceOf[NonTerminal].singleAttribute))
            val tempRemovedEntry = symbolTable.removed(idToken.get.asInstanceOf[TokenAttribute].lex)
            tempRemovedEntry++mapEntry
          }
          else symbolTable
        }

        val insertedContent = s"\t${nonTerminalInStack.get.asInstanceOf[NonTerminal].singleAttribute} ${idToken.get.asInstanceOf[TokenAttribute].lex};\n"

        SemanticInfo(semanticStack.empty, sequencer, updatedSymbolTable, content.concat(insertedContent), tempContent)

      case 7 =>
        //obtenho o token id.
        val tokenWithIntClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithIntClassification.t_type, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content, tempContent)

      case 8 =>
        val tokenWithRealClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithRealClassification.t_type, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content, tempContent)

      case 9 =>
        val tokenWithLiteralClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithLiteralClassification.t_type, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content, tempContent)

      case 11 =>
        val tokenWithIdClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] &&
            element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        if(tokenWithIdClassification.isDefined && !tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type.isEmpty){
          val tknType = tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type
          val tknLex = tokenWithIdClassification.get.asInstanceOf[TokenAttribute].lex
          val insertedContent = tknType match {
            case "int" => "\tscanf(\"" + "%d\", &" + tknLex + ");\n"
            case "real" => "\tscanf(\"" + "%lf\", &" + tknLex + ");\n"
            case "literal" => "\tscanf(\"" + "%s\", " + tknLex + ");\n"
          }
          SemanticInfo(semanticStack.empty, sequencer, symbolTable, content.concat(insertedContent), tempContent)
        }
        else{
          return null
        }


      case 12 =>
        val nonTerminalArg = semanticStack.find(element => element.isInstanceOf[NonTerminal])

        val insertedContent = s"\tprintf(${nonTerminalArg.get.asInstanceOf[NonTerminal].singleAttribute});\n"

        SemanticInfo(semanticStack.empty, sequencer, symbolTable, content.concat(insertedContent), tempContent)

      case 13 =>
        val tokenWithLiteralClassification: TokenAttribute = semanticStack.head.asInstanceOf[TokenAttribute]

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, tokenWithLiteralClassification.lex, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content, tempContent)

      case 15 =>
        val tokenWithIdClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] &&
            element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        val tokenType = tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type

        val temporaryText: String = tokenType match{
          case "real" => "\"%lf\", " + tokenWithIdClassification.get.asInstanceOf[TokenAttribute].lex
          case "int" =>  "\"%d\", " + tokenWithIdClassification.get.asInstanceOf[TokenAttribute].lex
          case "literal" =>  "\"%s\", " + tokenWithIdClassification.get.asInstanceOf[TokenAttribute].lex
        }

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs,
            nonTerminal.rhs, temporaryText, None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content, tempContent)

      case 17 =>
        val tokenWithIdClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] &&
            element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        val tokenWithRCBClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] && element.asInstanceOf[TokenAttribute].classification.equals(Token.ATTR))

        val symbolWithLDClassification = semanticStack.find(element => element.isInstanceOf[NonTerminal]).head

        if(symbolWithLDClassification.asInstanceOf[NonTerminal]
          .multiAttributes.asInstanceOf[TokenAttribute].t_type
              .equals(
                tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type)
        ) println("su su sucesso!")

        val insertedContent = s"\t${tokenWithIdClassification.get.asInstanceOf[TokenAttribute].lex} = " +
          s"${symbolWithLDClassification.asInstanceOf[NonTerminal].multiAttributes.asInstanceOf[TokenAttribute].lex};\n"


        SemanticInfo(semanticStack.empty, sequencer, symbolTable, content.concat(insertedContent), tempContent)


      case 18 =>
        //symbols with OPRD classification
        val symbolsWithOPRDClassification = semanticStack.filter(element =>
          element.isInstanceOf[NonTerminal]).reverse

        //obtém o operador matemático
        val tokenWithOPMClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] && element.asInstanceOf[TokenAttribute].classification.equals(Token.MATH_OPERATOR))

        val tempVar = s"T$sequencer"

        val temporaryExpression =
          s"$tempVar = " +
          symbolsWithOPRDClassification.map(element =>
          element.asInstanceOf[NonTerminal].multiAttributes.asInstanceOf[TokenAttribute].lex)
          .mkString(tokenWithOPMClassification.get.asInstanceOf[TokenAttribute].t_type) + "";

        val insertedContent = s"\t${temporaryExpression};\n"
        val insertedTempVariable = s"\t" + symbolsWithOPRDClassification.
          head.asInstanceOf[NonTerminal].multiAttributes.asInstanceOf[TokenAttribute].t_type + s" T$sequencer;\n"

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs,
            nonTerminal.singleAttribute,
            TokenAttribute(lex = s"T$sequencer",
              Token.ID,
              symbolsWithOPRDClassification.head.asInstanceOf[NonTerminal].multiAttributes.asInstanceOf[TokenAttribute].t_type))::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer + 1, symbolTable, content.concat(insertedContent), tempContent.concat(insertedTempVariable))

      case 19 =>
        val symbolWithOPRDClassification = semanticStack.head;

        val updatedSemanticStack = NonTerminal(nonTerminal.lhs, nonTerminal.rhs,
          nonTerminal.singleAttribute, symbolWithOPRDClassification.asInstanceOf[NonTerminal].multiAttributes)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content, tempContent)

      case 20 =>
        val tokenWithIdClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] &&
            element.asInstanceOf[TokenAttribute].classification.equals(Token.ID))

        if(tokenWithIdClassification.isDefined && !tokenWithIdClassification.get.asInstanceOf[TokenAttribute].t_type.isEmpty){
          println("Instancia ok!")

          val updatedSemanticStack =
            NonTerminal(nonTerminal.lhs, nonTerminal.rhs, nonTerminal.singleAttribute, tokenWithIdClassification.get.asInstanceOf[TokenAttribute])::semanticStack

          SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content, tempContent)
        }
        else
          //todo LANÇAR EXCEÇÃO
          return null

      case 21 =>
        val tokenWithNumClassification = semanticStack.head

        val updatedSemanticStack =
          NonTerminal(nonTerminal.lhs, nonTerminal.rhs, nonTerminal.singleAttribute, tokenWithNumClassification.asInstanceOf[TokenAttribute])::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content,  tempContent)

      case 23 =>
        val insertedContent = "\t}\n"
        SemanticInfo(semanticStack, sequencer, symbolTable, content.concat(insertedContent), tempContent)

      case 24 =>
        val symbolWithEXPRClassification =
          semanticStack.find(element => element.isInstanceOf[NonTerminal]).head

        val insertedContent = s"\tif (${symbolWithEXPRClassification.asInstanceOf[NonTerminal].singleAttribute}){\n"

        SemanticInfo(semanticStack.empty, sequencer+1, symbolTable, content.concat(insertedContent), tempContent)

      case 25 =>
        //obtém os operadores na ordem de inserção
        val symbolsWithOPRDClassification = semanticStack.filter(element =>
          element.isInstanceOf[NonTerminal]).reverse

        //obtém o operador relacional
        val tokenWithOPRClassification = semanticStack.find(element =>
          element.isInstanceOf[TokenAttribute] && element.asInstanceOf[TokenAttribute].classification.equals(Token.REL_OPERATOR))

        val types = symbolsWithOPRDClassification.map(t => t.asInstanceOf[NonTerminal].multiAttributes.asInstanceOf[TokenAttribute].t_type)

        //verifica se os tipos são iguais
        val typeResultComparison: Boolean = types.forall(element => element.equals(types.head))


        val temporaryExpression = symbolsWithOPRDClassification
          .map(tkn => tkn.asInstanceOf[NonTerminal].multiAttributes.asInstanceOf[TokenAttribute].lex)
          .mkString(tokenWithOPRClassification.get.asInstanceOf[TokenAttribute].lex)

        val insertedTempVariable = s"\t" + symbolsWithOPRDClassification.head
          .asInstanceOf[NonTerminal].multiAttributes.asInstanceOf[TokenAttribute].t_type + s" T$sequencer;\n"

        val insertedContent = s"\tT$sequencer = ${temporaryExpression};\n"

        val updatedSemanticStack =
        NonTerminal(nonTerminal.lhs, nonTerminal.rhs, s"T${sequencer}", None)::semanticStack

        SemanticInfo(updatedSemanticStack, sequencer, symbolTable, content.concat(insertedContent), tempContent.concat(insertedTempVariable))

      case 29 =>
        SemanticInfo(semanticStack.empty, sequencer, symbolTable, content, tempContent)

      case 30 =>
        SemanticInfo(semanticStack, sequencer, symbolTable, content, tempContent)
      case __ =>
        SemanticInfo(semanticStack, sequencer, symbolTable, content, tempContent)
    }
  }
}
