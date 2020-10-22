package Syntactic

import Utils.CSV_Processor

import scala.io.Source

object Parser {
  type Transition = Map[Int, Map[String, String]]

  private def getAutomataTransition(pathToFile: String):Transition = {
    try{
      val fileSource = Source.fromResource(pathToFile)

      val strList = fileSource.getLines().toList

      fileSource.close()

      CSV_Processor.getTransitionTable(0, strList.drop(1), strList.head, Map.empty)
    }catch{
      case e: Exception => Map.empty
    }

  }

  def init():Unit = {
    val actionTransition = getAutomataTransition("action.csv")
    val gotoTransition = getAutomataTransition("goto.csv")

    println(actionTransition(58))

  }




}
