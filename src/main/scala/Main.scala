
import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException
import AST._
import simplifier.Simplifier._

import simplifier.Simplifier

object Main {

  def parseString(str: String, parser: Parser): Node = {

    val parseResult = parser.parseAll(parser.program, str+"\n")

    parseResult match {
       case parser.Success(result: List[AST.Node], in) => 
         {
           println ("before: " + NodeList(result))
           simplify(NodeList(result))
         }
       case parser.NoSuccess(msg: String, in) => throw new IllegalArgumentException("FAILURE Could not parse '" + str + "': " + msg)
    }
  }
  
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
      //println(parseString("(x,y)+(u,v)",parser))
      //println(parseString("x**2+2*x*y+y**2",parser))
      //println(parseString("not True",parser))
      println(parseString("(x + y + k +z)/(k + x + z + y)",parser))
      //println(parseString("x**y*x**z",parser))
  }
}
