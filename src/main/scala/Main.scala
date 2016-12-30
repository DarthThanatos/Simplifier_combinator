
import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException
import AST._
import simplifier.Simplifier._

import simplifier.Simplifier

object Main {
  
  def parseGcd(args : Array[String]){
     if(args.length == 0) {
        println("Usage: sbt \"run filename ...\""); 
        return
     }
     val parser = new Parser()
     for (arg <- args) {
        try {
            println("Parsing file: " + arg)     
            val reader = new FileReader(arg)
            val parseResult = parser.parseAll(reader)
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
               case parser.Success(result , in) => {println(result)}
               case parser.NoSuccess(msg: String, in) => println("FAILURE " + parseResult)
            }
        }
        catch {
            case ex: FileNotFoundException => println("Couldn't open file " + arg)
            case ex: IOException => println("Couldn't read file " + arg)
        }
    }
  }
  
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
      val parser = new Parser()
      println("after: " + parseString("2**3**2",parser) + " \n")
      println("after: " + parseString("(x,y)+(u,v)",parser) + "\n")
      println("after: " + parseString("x**0",parser) + "\n")
      println("after: " + parseString("(x+y)**2",parser) + "\n")
      println("after: " + parseString("((x+y)**2)**2 + 2*(x+y)**2*z + z**2 + 2**3**2",parser) + "\n")
      println("after: " + parseString("x**2+2*x*y+y**2",parser) + "\n")
      println("after: " + parseString("(x+y)**2-x**2-2*x*y",parser) + "\n")
      println("after: " + parseString("(x+y)**2-(x-y)**2",parser) + "\n")
      println("after: " + parseString("4*x*y",parser) + "\n")
      println("after: " + parseString("(x**n)**m",parser) + "\n")
      println("after: " + parseString("x/x",parser) + "\n")
      println("after: " + parseString("(x+y*z)/(x+y*z)",parser) + "\n")
      println("after: " + parseString("(x+y)/(y+x)",parser) + "\n")
      println("after: " + parseString("(x+y*z)/(y*z+x)",parser) + "\n")
      println("after: " + parseString("1/(1/x)",parser) + "\n")
      println("after: " + parseString("1/(1/(x-z))",parser) + "\n")
      println("after: " + parseString("x-x",parser) + "\n")
      println("after: " + parseString(" not not not  x",parser) + "\n")
      println("after: " + parseString("{ \"a\": 1, \"b\": 2, \"a\": 3 }",parser) + "\n")
      println("after: " + parseString("x=x",parser) + "\n")
      
      val if_stmt_str = """if %s:
             { 
                x = 1
             }
             else: 
             {
                x = 0
             } """
            
            
      println("after: " + parseString(if_stmt_str.format("True"),parser) + "\n")
      println("after: " + parseString(if_stmt_str.format("False"),parser) + "\n")
      
      val if_expr_str = "x = y if %s else z"
      println("after: " + parseString(if_expr_str.format("True"),parser) + "\n")
      
      val str = """while False:
             { 
                x = x + 1
             } """
            
      println("after: " + parseString(str,parser) + "\n")
      println("after: " + parseString("",parser) + "\n")
      println("after: " + parseString("x*y+x*z+v*y+v*z",parser) + "\n")
      println("after: " + parseString(" -- x -- y",parser) + "\n")
      //println("after: " + parseString("---not--x",parser) + "\n")
      println("after: " + parseString("x=1;x=0;x=3;y = 10 + 3",parser) + "\n")
      println("after: " + parseString("-(x*(1/y))",parser) + "\n")
  }
}
