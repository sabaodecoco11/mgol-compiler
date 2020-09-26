import Utils.Common

import scala.annotation.tailrec
import scala.io.Source


object Compiler extends App{

  //arquivo inicial para testes...
  val filePath = "/home/sabaodecoco/estudos/compiladores/testeCompiladores"

  try {
    val fileSource: Source = Source.fromFile(filePath)
    val init = System.currentTimeMillis();

    val lines = fileSource.getLines();

    val content = getStringFromLines(lines, "")

    Scanner.begin(content, 0, content.size, 1, 0, 0, "", Common.getSymbolTable())

    val exit = System.currentTimeMillis();

    fileSource.close()

    println("\nExecution time: " + (exit - init) + "ms")

  }catch{
    case e:
        Exception => println("Deu ruim " + e.getMessage)
  }

  @tailrec
  def getStringFromLines(lines: Iterator[String], str: String): String ={
    if(lines.hasNext){
      getStringFromLines(lines.drop(0), str + lines.next() + "\n" )
    }
    else
      str + "eof"
  }



}





