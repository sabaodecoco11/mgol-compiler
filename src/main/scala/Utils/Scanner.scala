package Utils

import DFA.Automata
import Enum.CharPattern.CharPattern
import Enum.Token.Token
import Enum.{CharPattern, Token}

import scala.annotation.tailrec

object Scanner {
  private val dfa = new Automata;

  private val wordRangeRangeList = ('a' to 'z').concat('A' to 'Z').toList
  private val numberRangeRangeList = ('0' to '9').toList
  private val ignoredRangeList = List(' ', '\n', '\t')

  @tailrec
  def begin(lines: Iterator[String], linePosition: Int): Unit= {
    if(lines.hasNext){

      if(true) {
        val linhatual = lines.next() + "\n" //é necessário concatenar \n, pois next() a remove da String
        doSomethingWithThemChars(linhatual, linePosition+1)
      };

      //chama o processamento da próxima linha
      begin(lines.drop(0), linePosition + 1);
    }
  }

  def doSomethingWithThemChars(str: String, line: Int): Unit ={
    var previousState = 0;
    var nextState = 0;

    var col = 1

    var tokenStr = ""

    str.foreach(c => {
      val charGroup: String = getCharGroup(c) //classifica o tipo do caractere

      previousState = nextState
      nextState = dfa.automataProcessing(charGroup, nextState)

      //caractere que não é ignorado ou incorreto, logo deve ser concatenado ao token
      if(nextState > 0)
        tokenStr += c

      //o caractere atual não foi reconhecido no estado passado, trate-o
      if(nextState.equals(-1)){
        //se o estado passado for de aceitação, insira o token
        if(dfa.rejectionStates.contains(previousState)){
          println(c + " <- erro")
        }
        if(dfa.acceptedStates.contains(previousState)){
          val token = getTokenByState(previousState)

          if(token.equals(Token.ID)) {
            //TODO: inserir token na tabela de símbolos, se já não estiver lá
          }


          tokenStr = ""
        }

        nextState = dfa.automataProcessing(charGroup, 0)//reinicie o estado para verificar se o caractere pertence à linguagem

        //se o caractere é válido e de aceitação, insira-o no token
        if(nextState > 0){
          tokenStr += c
        }

      }

      if(nextState.equals(-2)){
        println("\n[Erro] no caractere: " + c + "\nlinha: " + line + " coluna " + col + "\n");
        //conforme requisito, se for obtido erro, o estado inicial deve ser retornado!
        nextState = 0
      }
      col = col + 1
    })
  }
  
  private def getTokenByState(state: Int): Token = {
    state match {
      case 1 => Token.OP_PARENTHESIS
      case 2 => Token.CL_PARENTHESIS
      case 3 => Token.SEMICOLON
      case 4 => Token.ID
      case 5 => Token.NUMBER
      case 8 => Token.NUMBER
      case 9 => Token.NUMBER
      case 11 => Token.COMMENT
      case 13 => Token.LITERAL
      case 14 => Token.MATH_OPERATOR
      case 15 => Token.REL_OPERATOR
      case 16 => Token.REL_OPERATOR
      case 17 => Token.REL_OPERATOR
      case 20 => Token.ATTR
      case 22 => Token.END_OF_FILE
      case 0 => None.get
    }
  }

  private def getCharGroup(c: Char): CharPattern = {
    if(wordRangeRangeList.contains(c))
      c.toLower.toString
    else if(ignoredRangeList.contains(c))
      CharPattern.IGNORED
    else if(numberRangeRangeList.contains(c))
      CharPattern.NUMERIC

    else if(c.equals('\n'))
      "\n"

    // retorna o próprio caractere
    else
      c.toString
  }


}
