import java.io.FileNotFoundException

import Utils.Scanner

import scala.io.Source


object LexicalAnalyzer extends App{

  //arquivo inicial para testes...
  val filePath = "/home/sabaodecoco/estudos/compiladores/testeCompiladores"


  try {
    val fileSource: Source = Source.fromFile(filePath)
    val init = System.currentTimeMillis();

    Scanner.begin(fileSource.getLines(), 0)

    val exit = System.currentTimeMillis();

    println("\nExecution time: " + (exit - init) + "ms")

  }catch{
    case e:
        Exception => println(e.getMessage)
  }





}





