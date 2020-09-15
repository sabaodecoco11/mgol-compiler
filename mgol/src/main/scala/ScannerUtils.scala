import scala.annotation.tailrec

object ScannerUtils {
  private val dfa = new DFA;

  @tailrec
  def begin(lines: Iterator[String], linePosition: Int): Unit= {
    if(lines.hasNext){

      if(true) {
        val linhatual = lines.next();
        println("esta é a string original: " + linhatual )
        doSomethingWithThemChars(linhatual)
      };

      //chama o processamento da próxima linha
      begin(lines.drop(0), linePosition + 1);
    }
  }

  def doSomethingWithThemChars(x:String) : Unit= {
    var estadoAnterior = 0
    var estado = 0
    x.foreach(c => {

      estadoAnterior = estado;
      print("estado anterior: " + estadoAnterior)
      print(" charactere: " + c)
      estado = dfa.automataProcessing(c, estado);

      print(" estado posterior: " + estado)
      print('\n')

      if(estado.equals(-1)){//não havia transição, tente novamente
        estadoAnterior = estado;
        estado = 0;
        print("\t[PROCESSAMENTO] estado anterior: " + estadoAnterior + "charactere: " + c)
        estado = dfa.automataProcessing(c, estado)
        print(" estado posterior: " + estado)
        print("\n\n")

        if(estado == estadoAnterior && estado == -1){
          println("ERRO no char : " + c)
        }
      }

      if(estado.equals(-2)){
        throw new Exception("Caractere inválido na linguagem!!!!!");
      }


    })

  }
}
