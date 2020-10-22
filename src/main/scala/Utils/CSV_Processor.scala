package Utils

import scala.annotation.tailrec

object CSV_Processor {

  @tailrec
  def getCsvInfo(linePos: Int, lines: List[String], csvColumn: String): Unit ={

    if(lines.length <= 0) {
      println("\nEnd!")
      return
    }

    println(s"\nESTADO: $linePos")
    val rowMap = processColumn(lines.head, 0,  linePos, csvColumn, 0, Map.empty[String, String], ",")

    println(rowMap)

    getCsvInfo(linePos + 1, lines.drop(1), csvColumn)

  }

  @tailrec
  def processColumn[K,V](row: String, rowIndex:Int,
                    state: Int,
                    csvColumn: String,
                    columnIndex: Int,
                    myMap : Map[String, String], separator: String): Map[String, String] ={

    if(columnIndex > csvColumn.length)
      return myMap;

    val firstCommaHead = csvColumn.indexOf(',', columnIndex)
    val firstCommaRow = row.indexOf(',', rowIndex)

    //get each row element
    if(firstCommaHead >= 0) {
      val element = csvColumn.substring(columnIndex, firstCommaHead)

      val rowElement = if(firstCommaRow >= 0) row.substring(rowIndex, firstCommaRow) else ""

      if(!rowElement.isEmpty && !rowElement.equals(separator) && !element.isEmpty && !element.equals(separator))
        println(s"$element" + s": $rowElement")

      val updatedMap = {
        if(!rowElement.isEmpty && !rowElement.equals(separator) && !element.isEmpty && !element.equals(separator))
          Map[String, String](element -> rowElement)
        else myMap
      }

      processColumn(row, firstCommaRow + 1, state, csvColumn, firstCommaHead + 1, updatedMap ++ myMap, separator)
    }

    //last element
    else{
      val columnElement = csvColumn.substring(columnIndex)
      val rowElement = row.substring(rowIndex)

      if(!columnElement.equals(",") && !rowElement.equals(",") && !rowElement.isEmpty && !columnElement.isEmpty)
        Map[String, String](columnElement-> rowElement) ++ myMap
      else myMap
    }

  }

}
