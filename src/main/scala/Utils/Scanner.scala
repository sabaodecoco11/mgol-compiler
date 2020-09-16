package Utils

import DFA.Automata
import Enum.CharPattern

import scala.annotation.tailrec

object Scanner {
  private val dfa = new Automata;

  private val wordRangeRangeList = ('a' to 'z').concat('A' to 'Z').toList
  private val numberRangeRangeList = ('0' to '9').toList
  private val logicalRangeList = ('<' to '>').toList
  private val ignoredRangeList = List(' ', ';', '\n', '\t')

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

  def doSomethingWithThemChars(str: String, i: Int): Unit ={
    var estadoAnterior = 0;
    var estadoPosterior = 0;

    var token = ""

    str.foreach(c => {
      val charGroup: String = getCharGroup(c) //classifica o tipo do caractere

      estadoAnterior = estadoPosterior
      estadoPosterior = dfa.automataProcessing(charGroup, estadoPosterior)

      if(estadoPosterior > 0)// li um caractere que não é ignorado ou incorreto, logo deve ser concatenado ao token
        token += c

      if(estadoPosterior.equals(-1)){//o caractere atual não foi reconhecido no estado passado, trate-o
        if(dfa.acceptedStates.contains(estadoAnterior)){//se o estado passado for de aceitação, insira o token
          println(token)
          token = ""
        }

        estadoAnterior = 0;//reinicie o estado para verificar se o caractere pertence à linguagem
        estadoPosterior = dfa.automataProcessing(charGroup, 0)

        //se o caractere é válido e de aceitação, insira-o no token
        if(estadoPosterior > 0 && dfa.acceptedStates.contains(estadoPosterior)){
          token += c
        }

        if(estadoPosterior.equals(-2)){
          throw new Exception("ERRO!");
        }
      }
    })
  }


//  @tailrec
//  def doSomethingWithThemChars(x:String, linha: Int, strSize: Int) : Unit= {
//    if(strSize > 0){
//      val c = x.charAt(0)
//
//
//    }
//
//    return doSomethingWithThemChars(x.drop(0), linha, x.size-1)
//  }




  private def getCharGroup(c: Char): String = {
    if(wordRangeRangeList.contains(c))
      CharPattern.LETTER
    else if(ignoredRangeList.contains(c))
      CharPattern.IGNORED
    else if(numberRangeRangeList.contains(c))
      CharPattern.NUMERIC
    else if(logicalRangeList.contains())
      CharPattern.NUMERIC

    else if(c.equals('\n'))
      "\n"

    else // retorna o próprio caractére
      c.toString
  }


}
