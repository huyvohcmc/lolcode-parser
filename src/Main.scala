import scala.io.Source

object Main extends App{
  println(new java.io.File(".").getCanonicalPath)
  val lines = Source.fromFile("../testcase/" + "Function.in").getLines
  val input = if (!lines.isEmpty) lines.reduceLeft[String](_ + '\n' + _) else ""
  println(input+"\n=====================\n")
  
  val lexical = new LolLexical
  val scanner = new lexical.Scanner(input)

  runAll(scanner)
 
  def runAll(scan: lexical.Scanner): Any =
    if (scan.atEnd) {
      println("EOF")
    }
    else {
      println(scan.first, scan.first.getClass.getSimpleName)
      runAll(scan.rest)
    }
  
  val parser = new LolParser
  val result = parser.parse(input)
  println(result)
  
}