
import Syntactic.Parser


import scala.annotation.tailrec
import scala.io.Source


object Compiler extends App{

  try {
    val fileSource: Source = Source.fromResource("input.alg")

    val lines = fileSource.getLines();

    val content = getStringFromLines(lines, "")

    val init = System.currentTimeMillis()
    Parser.init(content)
    val end = System.currentTimeMillis()

    println( "elapsed time: " + (end-init) + "ms")

    fileSource.close()

  }catch{
    case e:
        Exception => println(e.getMessage)
  }

  @tailrec
  def getStringFromLines(lines: Iterator[String], str: String): String ={
    if(lines.hasNext)
      getStringFromLines(lines.drop(0), str + lines.next() + "\n" )

    else str
  }

//  @tailrec
//  def getAllTokens(content: String, strPos: Int, strSize: Int, line: Int, column: Int,  previousState: Int, lexeme: String, symbolTable: SymbolTable): Unit = {
//    val lexData = Scanner.getToken(content,  strPos, content.size, line,  previousState, "", column, Common.getSymbolTable())
//
//    //mostra o lexema, token e tipo
//    for((k,v) <- lexData.recognizedToken)
//      println(k.replace("\n", " ") + " " + v._1 + " " + v._2)
//
//    if(lexData.recognizedToken.contains("EOF")) return;
//
//    else
//      getAllTokens(content, lexData.lastPos, content.size, lexData.line, lexData.column, lexData.state, "", lexData.updatedSymbolTable)
//  }



}





