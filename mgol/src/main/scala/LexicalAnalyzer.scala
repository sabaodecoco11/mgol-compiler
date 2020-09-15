import java.io.FileNotFoundException

import scala.io.Source
import ScannerUtils.begin;

object LexicalAnalyzer extends App{

  //arquivo inicial para testes...
  val filePath = "/home/sabaodecoco/estudos/compiladores/testeCompiladores"

  val myDfa = new DFA

  try {
    val fileSource: Source = Source.fromFile(filePath);

    ScannerUtils.begin(fileSource.getLines(), 0);

  }catch{
    case e:
        Exception => println(e.getMessage)
  }





}





