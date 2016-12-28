package simplifier

import AST._
import com.sun.javafx.fxml.expression.BinaryExpression

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

  def simplify(node: BinExpr) = {
    println("Bin expr: " + node.left + " " + node.right)
    node match{
      case BinExpr("+", CustomizedTuple(tup1:NodeList),CustomizedTuple(tup2:NodeList)) =>{
        CustomizedTuple(NodeList(tup1.list ++ tup2.list))
      }
      
      case BinExpr("/", val1 : Node, val2: Node) if val1 == val2 => IntNum(1)
      
      case BinExpr("*", IntNum(int1), IntNum(int2)) => IntNum(int1 * int2)
      case BinExpr("/", IntNum(int1), IntNum(int2)) => IntNum(int1 / int2)
      case BinExpr("+", IntNum(int1), IntNum(int2)) => IntNum(int1 + int2)
      case BinExpr("-", IntNum(int1), IntNum(int2)) => IntNum(int1 - int2)
      case BinExpr("**", IntNum(int1), IntNum(int2))  => IntNum(scala.math.pow(int1.toDouble, int2.toDouble).toInt)
      
      case BinExpr( _ : String, left : Variable, right: Variable) => simplifyVariableOpers(node)
    }
  }
  
  def simplifyVariableOpers(node : BinExpr) = node match {
    case BinExpr("*", Variable(x),Variable(y)) => Variable( if (x < y)  x + y else y + x )
    case BinExpr("+", Variable(x), Variable(y)) => Variable (if (x < y) x + "plus" + y else y + "plus" + x) 
    case BinExpr("/", Variable(x), Variable(y)) if (x == y) => IntNum(1)
  }
  
  def simplifyBinRek(node: BinExpr) : Node = node match{
    case BinExpr(op : String, left : BinExpr, right: BinExpr) => {
      println("bin rek both")
      val simplified_left: Node = simplifyBinRek(left)
      val simplified_right : Node = simplifyBinRek(right)
      simplify(BinExpr(op, simplified_left, simplified_right))
    }
    case BinExpr(op : String, left : Node ,right: BinExpr) => {
      val simplified_right : Node = simplifyBinRek(right) 
      simplify(BinExpr(op, left, simplified_right))      
    }
    case BinExpr(op : String, left : BinExpr, right: Node) => {
      val simplified_left : Node = simplifyBinRek(left)
      simplify(BinExpr(op, simplified_left,right))
    }
    case BinExpr( _ : String, left : Variable, right: Variable) => simplifyVariableOpers(node)
    case _ : Node => simplify(node)
  }
  
  
  def simplify(node_list : NodeList) : NodeList= {
    println("node list")
    NodeList(for (x <- node_list.list)yield simplify(x)) 
    
  }
  
  def simplify(tuple: CustomizedTuple) : CustomizedTuple= {
    println("custom tuple")
    CustomizedTuple(simplify(tuple.list))
  }
  
  def simplify(variable: Variable) = variable
  
  def simplify(node : Unary) = {
    println("sim un")
    node match {
    case Unary("not", t: TrueConst) => FalseConst()
    case Unary("not", f: FalseConst) => TrueConst()
  }}
  
  def simplify(node: Node): Node = {
   
    println("sim node" + node.getClass)
    node match{ 
      case tuple : CustomizedTuple => simplify(tuple)
      case bin_expr : BinExpr => simplifyBinRek(bin_expr)
      case unary_expr : Unary => simplify(unary_expr)
      case node_list: NodeList => simplify(node_list)
      case variable : Variable => simplify(variable)
      case t : Node => t
    }
  }
}
