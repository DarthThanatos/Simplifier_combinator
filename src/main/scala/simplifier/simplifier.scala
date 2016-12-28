package simplifier

import AST._
import com.sun.javafx.fxml.expression.BinaryExpression

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

  def simplify(node: BinExpr) = {
    println("Bin expr" + node.left + " " + node.right)
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
      
      case BinExpr("**", IntNum(int1), IntNum(int2))  => {
          var res = simplifyIntPow(int1,int2)
          println("Bin expr ints" + res)
          res
      }
    }
  }
  
  def simplifyBinRek(node: BinExpr) : Node = node match{
    case BinExpr(_, left : IntNum ,right: BinExpr) => {IntNum(
        scala.math.pow(left.value.toDouble, 
            simplifyBinRek(right).asInstanceOf[IntNum].value.toDouble).toInt)}
    case _ : Node => simplify(node)
  }
  
  def simplifyIntPow(int1 : Int,int2: Int) =  IntNum(scala.math.pow(int1.toDouble,int2.toDouble).toInt)
  
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
    case bin_expr : BinExpr => simplifyBinRek(bin_expr)
    case node_list: NodeList => simplify(node_list)
    case variable : Variable => simplify(variable)
  }

  

}
