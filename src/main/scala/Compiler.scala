import scala.io.Source


object Compiler extends App{

  //arquivo inicial para testes...
  val filePath = "/home/sabaodecoco/estudos/compiladores/testeCompiladores"

  try {
    val fileSource: Source = Source.fromFile(filePath)
    val init = System.currentTimeMillis();

    Scanner.begin(fileSource.getLines(), Utils.Common.getSymbolTable(), 0)

    val exit = System.currentTimeMillis();

    fileSource.close()

    println("\nExecution time: " + (exit - init) + "ms")

  }catch{
    case e:
        Exception => println("Deu ruim " + e.getMessage)
  }





}





