package simplifier

import AST._
import com.sun.javafx.fxml.expression.BinaryExpression

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

  def simplify(node: BinExpr) = {
    //println("Bin expr")
    node match{
      case BinExpr("+", CustomizedTuple(tup1:NodeList),CustomizedTuple(tup2:NodeList)) =>{
        CustomizedTuple(NodeList(tup1.list ++ tup2.list))
      }
      
      case BinExpr("+", StringConst(str1), StringConst(str2)) => {
        StringConst(str1 + str2)
      }
      case BinExpr("*", StringConst(str1), IntNum(int1)) => {
        StringConst(str1 * int1)
      }
      
      case BinExpr("**", Variable(int1), Variable(int2)) if int1 == int2 => {
          Variable(int1)
      }
    }
  }
  
  def simplify(node_list : NodeList) : NodeList= {
    //println("node list")
    NodeList(for (x <- node_list.list)yield simplify(x)) 
    
  }
  
  def simplify(tuple: CustomizedTuple) : CustomizedTuple= {
    //println("custom tuple")
    CustomizedTuple(simplify(tuple.list))
  }
  
  def simplify(variable: Variable) = variable
  
  def simplify(node: Node): Node = node match{ 
    case tuple : CustomizedTuple => simplify(tuple)
    case bin_expr : BinExpr => simplify(bin_expr)
    case node_list: NodeList => simplify(node_list)
    case variable : Variable => simplify(variable)
  }

  

}
