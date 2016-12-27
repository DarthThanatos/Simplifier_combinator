
import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException

import simplifier.Simplifier

object Main {

  def main(args: Array[String]) {
      if(args.length == 0) {
        println("Usage: sbt \"run filename ...\""); 
        return
      }

      val parser = new Parser()

      for (arg <- args) {
          try {
              println("Parsing file: " + arg)     
              //val reader = new FileReader(arg)
              //val parseResult = parser.parseAll(reader)
              val parseResult = parser.parseAll(parser.const,"True")
              parseResult match {
                 case parser.Success(result: List[AST.Node], in) => {
                     println("\nAST:")
                     println(parseResult)
                     val tree = AST.NodeList(result)
                     val simplifiedTree = simplifier.Simplifier.simplify(tree)
                     println("\nAST after optimization:")
                     println(simplifiedTree)
                     println("\nProgram after optimization:")
                     println(simplifiedTree.toStr)
                 }
                 case parser.Success(result : AST.TrueConst, in) => {println(result.toStr)}
                 case parser.NoSuccess(msg: String, in) => println("FAILURE " + parseResult)
              }
              println("Hello" + parser.stringLiteral.toString())
          }
          catch {
              case ex: FileNotFoundException => println("Couldn't open file " + arg)
              case ex: IOException => println("Couldn't read file " + arg)
          }
      }
  }
}
