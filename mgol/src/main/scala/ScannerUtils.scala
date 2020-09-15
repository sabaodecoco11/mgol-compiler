import scala.annotation.tailrec

object ScannerUtils {
  private val dfa = new DFA;

  @tailrec
  def begin(lines: Iterator[String], linePosition: Int): Unit= {
    if(lines.hasNext){

      doSomethingWithThemChars(lines.next());

      //chama o processamento da próxima linha
      begin(lines.drop(0), linePosition + 1);
    }
  }

  def doSomethingWithThemChars(x:String) : Unit= {
    x.foreach(c => {

    })

  }
}
