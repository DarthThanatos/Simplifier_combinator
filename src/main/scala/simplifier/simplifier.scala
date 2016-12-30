package simplifier

import AST._
import com.sun.javafx.fxml.expression.BinaryExpression

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

  
  def simplifyBinRek(node: BinExpr) : Node = {
      println("bin rek: " + node)
      node match{
      //parseString("x*y + x*z + v*y + v*z") mustEqual parseString("(x+v)*(y+z)")
      case BinExpr("+",BinExpr("+", BinExpr("+", BinExpr("*",x1,y1),BinExpr("*",x2,z1)),BinExpr("*",v1,y2)),BinExpr("*",v2,z2)) 
      if x1 == x2 && y1 == y2 && z1 == z2 && v1 == v2 => 
        BinExpr("*", BinExpr("+",x1,v1), BinExpr("+",y1,z1))
      case BinExpr(op, x,y) => simplify(BinExpr(op, simplify(x),simplify(y)))
      //case _ : Node => simplify(node)
    }
  }
  
  def simplify(node: BinExpr) = {
    //println("Bin expr: " + node.left + " " + node.right)
    node match{
      case BinExpr("+", CustomizedTuple(tup1:NodeList),CustomizedTuple(tup2:NodeList)) =>{
        CustomizedTuple(NodeList(tup1.list ++ tup2.list))
      }
      //parseString("[]+[]") mustEqual parseString("[]")
      case BinExpr("+",ElemList(List()),ElemList(List())) => ElemList(List())
      //parseString("[a,b,c]+[]") mustEqual parseString("[a,b,c]")
      case BinExpr("+", ElemList(x), ElemList(List())) if x != List()=> ElemList(x)
      //parseString("[]+[a,b,c]") mustEqual parseString("[a,b,c]")
      case BinExpr("+", ElemList(List()), ElemList(x)) if x != List()=> ElemList(x)
      //parseString("[a,b,c]+[x,y]") mustEqual parseString("[a,b,c,x,y]")
      case BinExpr("+", ElemList(x), ElemList(y)) => ElemList(x ++ y)
      
      
      case BinExpr("*", IntNum(int1), IntNum(int2)) => IntNum(int1 * int2)
      case BinExpr("/", IntNum(int1), IntNum(int2)) => IntNum(int1 / int2)
      case BinExpr("+", IntNum(int1), IntNum(int2)) => IntNum(int1 + int2)
      case BinExpr("-", IntNum(int1), IntNum(int2)) => IntNum(int1 - int2)
      case BinExpr("**", IntNum(int1), IntNum(int2))  => IntNum(scala.math.pow(int1.toDouble, int2.toDouble).toInt)
      case _ => simplifyPowerLaws(node)
    }
  }
  
  def simplifyPowerLaws(node : BinExpr) = node match{
    case BinExpr("**", x ,IntNum(0)) => IntNum(1)
    case BinExpr("**", x, IntNum(1)) => x
    case BinExpr("*", BinExpr("**",Variable(x), Variable(y)), BinExpr("**",Variable(u),Variable(v))) if x == u=>
      BinExpr("**",Variable(x),BinExpr("+", Variable(y),Variable(v)))
    case 
    BinExpr("+", BinExpr( "+", BinExpr( "**", x,  IntNum(2)), BinExpr("*",BinExpr("*",IntNum(2), y ),z)), BinExpr( "**",u, IntNum(2)) ) 
            if (x == y && z == u) => BinExpr("**",BinExpr("+",x,z), IntNum(2))
    case BinExpr("**", BinExpr("**",Variable(x),Variable(n)),Variable(m)) => BinExpr("**",Variable(x),BinExpr("*",Variable(n), Variable(m)))
    //"(x+y)**2-x**2-2*x*y" => "y**2"
    case 
    BinExpr("-",BinExpr("-", BinExpr("**",BinExpr("+", x1,y1),IntNum(2)),BinExpr( "**",x2,IntNum(2))),BinExpr( "*", BinExpr( "*",IntNum(2),x3 ), y3)) 
      if(x1 == x3 && x1 == x2 && y1 == y3) => BinExpr("**", y1, IntNum(2))
    //"(x+y)**2-(x-y)**2" => "4*x*y"
    case
    BinExpr("-",BinExpr("**", BinExpr( "+", x1,y1),IntNum(2)),BinExpr("**", BinExpr("-",x2,y2),IntNum(2)) ) 
      if(x1 == x2 && y1 == y2) => BinExpr("*",BinExpr("*",IntNum(4),x1),y1)   
    case _ => simplifyDivisionLaws(node)
  }
   
  def simplifyDivisionLaws(node : BinExpr) = node match{
    case BinExpr("/", x, y) if x==y => IntNum(1)   
    //parseString("(x+y)/(y+x)") mustEqual parseString("1")   
    case BinExpr("/",BinExpr("+",x1,y1),BinExpr("+",y2,x2)) => IntNum(1)
    //parseString("1/(1/x)") mustEqual parseString("x")
    case BinExpr("/",IntNum(1), BinExpr("/",IntNum(1),x)) => x
    //parseString("x*(1/y)") mustEqual parseString("x/y")
    case BinExpr("*", x, BinExpr("/",IntNum(1),y)) => BinExpr("/",x,y)
    case _ => simplifyExpressions(node)
  }
  
  
  def simplifyExpressions(node : BinExpr) = node match{
    //parseString("x+0") mustEqual parseString("x")
    case BinExpr("+",x,IntNum(0)) => x
    //parseString("0+x") mustEqual parseString("x")
    case BinExpr("+",IntNum(0), x) => x
    //parseString("x-x") mustEqual parseString("0")
    case BinExpr("-",x,y) if x == y => IntNum(0)
    //parseString("-x+x") mustEqual parseString("0")
    case BinExpr("+",Unary("-",x),y) if x == y => IntNum(0)
    //parseString("x*1") mustEqual parseString("x")
    case BinExpr("*",x,IntNum(1)) => x
    //parseString("1*x") mustEqual parseString("x")
    case BinExpr("*",IntNum(1),x) => x
    //parseString("0*x") mustEqual parseString("0")
    case BinExpr("*", IntNum(0), x) => IntNum(0)
    //parseString("x or x") mustEqual parseString("x")
    case BinExpr("or", x,y) if x == y => x
    //parseString("x and x") mustEqual parseString("x")
    case BinExpr("and", x,y) if x == y => x    
    //parseString("x or True") mustEqual parseString("True")
    case BinExpr("or", x, TrueConst()) => TrueConst()
    //parseString("x or False") mustEqual parseString("x")
    case BinExpr("or", x, FalseConst()) => x
    //parseString("x and False") mustEqual parseString("False")
    case BinExpr("and", x, FalseConst()) => FalseConst()
    //parseString("x and True") mustEqual parseString("x")
    case BinExpr("and", x, TrueConst()) => x
    //parseString("x==x") mustEqual parseString("True")
    //parseString("x>=x") mustEqual parseString("True")
    //parseString("x<=x") mustEqual parseString("True")
    //parseString("x!=x") mustEqual parseString("False")
    //parseString("x>x") mustEqual parseString("False")
    //parseString("x<x") mustEqual parseString("False")
    case BinExpr(op:String, x,y) if x == y => {
      val trueOps : List[String] = List("==",">=","<=")
      val res =  if (trueOps.contains(op)) TrueConst() else FalseConst()
      res
    }
    case _ => simplifyCommutativity(node)
  }
  
  def simplifyCommutativity(node : BinExpr) = node match{
    //parseString("x+5-x") mustEqual parseString("5")
    case BinExpr("-",BinExpr("+",x, y),z) if x == z => y 
    //parseString("(a or b) and (b or a)") mustEqual parseString("a or b")
    case BinExpr("and", BinExpr("or",a1,b1), BinExpr("or",b2,a2)) if (a1 == a2 && b1 == b2) => BinExpr("or",a1,b1) 
    //parseString("(a and b) or (b and a)") mustEqual parseString("a and b")
    case BinExpr("or", BinExpr("and",a1,b1), BinExpr("and",b2,a2)) if (a1 == a2 && b1 == b2) => BinExpr("and",a1,b1) 
    
    case _ => simplifyDistrMult(node)
  }
  
  
  def simplifyDistrMult(node : BinExpr) = node match{
    //parseString("2*x-x") mustEqual parseString("x")
    case BinExpr("-", BinExpr("*", IntNum(2),x), y) if x == y => x 
    //parseString("x*z+y*z") mustEqual parseString("(x+y)*z")
    case BinExpr("+", BinExpr("*",x,z1), BinExpr("*",y,z2)) if z1 == z2 => BinExpr("*",BinExpr("+",x,y),z1)
    //parseString("x*y+x*z") mustEqual parseString("x*(y+z)")
    case BinExpr("+", BinExpr("*",x1,y), BinExpr("*",x2,z)) if x1 == x2 => BinExpr("*",x1, BinExpr("+",y,z))

    case _ => node
  }
  
  
  /*
   * // in case simplify below does not work :)  
   def simplify(node_list : NodeList) : Node = node_list match{
    //println("node list")
    case NodeList(List(Assignment(x1,a), Assignment(x2,b))) if x1 == x2 => NodeList(List(Assignment(x1,b)))
    case _ => {
      val res = NodeList(
          {
            var newlist = for (x <- node_list.list) yield simplify(x)
            newlist = newlist diff newlist.collect{case e: EmptyNode => e}
            newlist 
          }
      )
      if(res.list == List()) EmptyNode() else res
    }
    
  }*/
  
  def simplify(node_list : NodeList) : Node = {
      val simplifiedList = node_list.list match {
        case (x @ Assignment(left1,right1)) :: (y @ Assignment(left2,right2)) :: t =>  
          if (left1 == left2) simplify(NodeList(List(y) ++ t)) match{
              case n : NodeList => n.list
              case c => List(c) 
          }
          else List(x) ++ {
            simplify(NodeList(List(y) ++ t)) match {
              case n : NodeList => n.list
              case c => List(c) 
            }
          }
        case h::t => List(simplify(h)) ++ {
          simplify(NodeList(t)) match {
              case n : NodeList => n.list
              case c => List(c) 
          }
        }
        case Nil => List[Node]()
      }
      val res = NodeList(
          {
            val newlist = simplifiedList diff simplifiedList.collect{case e: EmptyNode => e}
            newlist 
          }
      )
      if(res.list == List()) EmptyNode() else res
    
  }
  
  def simplify(tuple: CustomizedTuple) : CustomizedTuple= {
    //println("custom tuple")
    CustomizedTuple(simplify(tuple.list).asInstanceOf[NodeList])
  }
  
  def simplify(variable: Variable) = variable
  
  def simplifyUnaryRek(node : Unary) : Node = node match{
    case Unary(op : String, x : Unary) => {
      val res = countOpers(op, x)
      if ((res._1 + 1) % 2 == 1) Unary(op, simplify(res._2)) else simplify(res._2)
    }
    case Unary(op, x) => simplify(Unary(op,simplify(x)))
    case _ => simplify(node) 
  }
  
  def countOpers(op: String, node: Unary) : (Int,Node) = 
      node match{
    
      case Unary(op, x : Unary) => {var tmp = countOpers(op, x); val res = (tmp._1 + 1, tmp._2); println("count " + res); res}
      case Unary(op, x : Node) => {println ("count 1"); (1,x)}
    }
  
  
  def simplify(node : Unary) = 
  {
      //println("sim un")
      node match {
      case Unary("not", t: TrueConst) => FalseConst()
      case Unary("not", f: FalseConst) => TrueConst()
      //parseString("not x==y") mustEqual parseString("x!=y")
      //parseString("not x!=y") mustEqual parseString("x==y")
      //parseString("not x>y") mustEqual parseString("x<=y")
      //parseString("not x<y") mustEqual parseString("x>=y")
      //parseString("not x>=y") mustEqual parseString("x<y")
      //parseString("not x<=y") mustEqual parseString("x>y")
      case Unary("not", BinExpr(op, left,right)) => {
        val notOperMap = Map("==" -> "!=", "!=" -> "==", ">" -> "<=", "<" -> ">=", ">=" -> "<", "<=" -> ">")
        BinExpr(notOperMap.get(op).orNull, left,right)
      }
      case _ => node
    }
  }
    
  
  def simplifyKeyDatmList(node : KeyDatumList) = {
    val alreadyUsed = scala.collection.mutable.Map[Node,Node]()
    for(x <- node.list) alreadyUsed(x.key) = x.value
    KeyDatumList(alreadyUsed.toList.map(x => KeyDatum(x._1, x._2)))
  }
  
  def simplify(node: IfElseExpr) :Node = {
    if (simplify(node.cond) == TrueConst())simplify(node.left) 
    else simplify(node.right)
  }
  
  def simplify(node : Assignment) : Node = node match{
    case Assignment(x,y) if x == y => EmptyNode()
    case Assignment(x, y : IfElseExpr) => Assignment(x, simplify(y))
    case Assignment(x,y) => Assignment(simplify(x),simplify(y))
    case _ => node
  }
  
  def simplify(node : IfElifElse) :Node= {
    val cond = simplify(node.cond)
    if (cond == TrueConst())simplify(node.if_body.asInstanceOf[NodeList].list.head) 
    else simplify(node.elseBody.asInstanceOf[NodeList].list.head)
  }
  
  def simplify(node : WhileInstr) : Node = {
    if (simplify(node.cond) == FalseConst()) EmptyNode()
    else simplify(node.body)
  }
  
  def simplify(node: Node): Node = {
   
    println("sim node" + node.getClass)
    node match{ 
      case tuple : CustomizedTuple => simplify(tuple)
      case bin_expr : BinExpr => simplifyBinRek(bin_expr)
      case unary_expr : Unary => simplifyUnaryRek(unary_expr)
      case node_list: NodeList => simplify(node_list)
      case variable : Variable => simplify(variable)
      case keydatumL : KeyDatumList => simplifyKeyDatmList(keydatumL)
      case assignment : Assignment => simplify(assignment)
      case if_elif : IfElifElse => simplify(if_elif)
      case while_instr : WhileInstr => simplify(while_instr)
      case t : Node => t
    }
  }
}
