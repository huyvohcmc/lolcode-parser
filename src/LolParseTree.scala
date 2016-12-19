trait PT {}

class ProgramPT(val statements: List[PT]) extends PT {
  override def toString = s"[${statements mkString(",")}]";
}

trait ExpressionPT extends StatementPT{}

class IdentPT(val id: String) extends ExpressionPT {
  override def toString = id
}

class ValuePT(val value: String) extends ExpressionPT {
  override def toString = value
}

trait StatementPT extends PT{}

class PrintPT(val expr:List[ExpressionPT], val newLine: Boolean) extends StatementPT {
  override def toString = if(newLine) s"println(${expr mkString(",")})" else s"print(${expr mkString(",")})"
}

class ReadPT(val ident:IdentPT) extends StatementPT {
  override def toString = s"read($ident)"
}

class VariableDeclarationPT(val name:IdentPT, val value:Option[ExpressionPT]) extends StatementPT {
  override def toString = {
    if (value== None) s"var($name)" else s"var($name,${value.get})"
  }
}

class AssignmentPT(val variable: IdentPT, val expression:ExpressionPT) extends StatementPT {
  override def toString = s"assign($variable,$expression)"
}

class UnaryOperatorPT(val operator: String, val expr: ExpressionPT) extends ExpressionPT {
  override def toString = {
    operator match {
      case "NOT" => s"not($expr)"
    }
  }
}

class BinaryOperatorPT(val operator: String, val expr1: ExpressionPT, val expr2: ExpressionPT) extends ExpressionPT {
  override def toString = {
    val op = operator match {
      case "SUM OF" => "sum"
      case "DIFF OF" => "diff"
      case "PRODUKT OF" =>"prod"
      case "QUOSHUNT OF" => "div"
      case "MOD OF" => "mod"
      case "BIGGR OF" => "max"
      case "SMALLR OF" => "min"
      case "BOTH OF" => "and"
      case "EITHER OF" => "or"
      case "WON OF" => "xor"
      case "BOTH SAEM" => "eq"
      case "DIFFRINT" => "neq"
    }
    s"$op($expr1,$expr2)"
  }
}

class MultiArityOperatorPT(val operator: String, val expr:List[ExpressionPT]) extends ExpressionPT{
 override def toString = {
   val op = operator match {
      case "ALL OF" => "all"
      case "ANY OF" =>"any"
    }
   s"$op($expr)"
 }
}

class BreakPT() extends StatementPT {
  override def toString = "break"
}
//IF ELSE
/*
 * <expression>
 * O RLY?
 * 		YA RLY
 * 			<code block>							ifCodeBlock
 * 		[MEBBE <expression>
 * 			<code block>							elseIfCodeBlock
 * 		[MEBBE <expression>
 * 			<code block>							elseIFCodeBlock
 * 		...]]
 * 		[NO WAI
 * 			<code block>]							elseCodeBlock
 * OIC
 */
class ElseIFPT(val expr:ExpressionPT, statements:List[StatementPT]) extends StatementPT {
  override def toString = s"[$expr,${statements mkString(",")}]"
}

class IfElsePT(val ifCodeBlock: List[StatementPT], val elseIfCodeBlock: Option[List[ElseIFPT]], val elseCodeBlock: Option[List[StatementPT]]) extends StatementPT {
  override def toString = s"ifelse([${ifCodeBlock mkString(",")}],${elseIfCodeBlock.getOrElse(List()) mkString(",")},[${elseCodeBlock.getOrElse(List())  mkString(",")}])"
}

//CASE
/*
 * WTF?
 * OMG <value literal>
 * 		<code block>
 * [OMG <value literal>
 * 		<code block> ...]
 * [OMGWTF
 * 		<code block>]
 * OIC
 */
class SwitchPT(val caseList: List[CasePT]) extends StatementPT {
  override def toString = s"switch(${caseList mkString(",")})"
}

trait CasePT extends StatementPT;

class ValueCasePT(val caseValue: List[ValuePT], val statements:List[StatementPT]) extends CasePT {
  override def toString = s"[[${caseValue mkString(",")}],[${statements mkString(",")}]]"
}

class DefaultCasePT(val statements:List[StatementPT]) extends CasePT {
  override def toString = s"[default,[${statements mkString(",")}]]"
}

//LOOP
/*
 * IM IN YR <label> [<operation> YR <variable> [TIL|WILE <expression>]]
 * 	<code block>
 * IM OUTTA YR <label>
 */
class LoopConditionPT(val operation: String, val variable: IdentPT, val condition: Option[(String,ExpressionPT)]) extends StatementPT {
  override def toString = {
    val conditionExpr = condition match {
      case Some(("TIL", exp)) => s"until($exp)"
      case Some(("WILE", exp)) => s"while($exp)"
      case _ => "while(true)"
    }
    val operationExpr = operation match {
      case "UPPIN" => s"inc($variable)"
      case "NERFIN" => s"dec($variable)"
      case other => s"$other($variable)"
    }
    s"$conditionExpr,$operationExpr"
  }
}

class LoopPT(val name: IdentPT, val loopCondition: Option[LoopConditionPT], val codeBlock:List[StatementPT]) extends StatementPT {
  override def toString = {
    loopCondition match {
      case Some(condition) => s"loop($name,$condition,[${codeBlock mkString(",")}])"
      case None => s"loop($name,while(true),[${codeBlock mkString(",")}])"
    }
  }
}

//FUNCTION
/*
 * HOW IZ I <function name> [YR <argument1> [AN YR <argument2> …]]
 * 	<code block>
 * IF U SAY SO
 */
class FunctionPT(val name: IdentPT, val args:List[IdentPT], val codeBlock: List[StatementPT]) extends StatementPT {
  override def toString = s"func($name,[${args mkString(",")}],[${codeBlock  mkString(",")}])"
}

/*
 * FOUND YR <expression>				return(expression)
 * GTFO													return(null)
 */
class FunctionReturnPT(val expr: Option[ExpressionPT]) extends StatementPT {
  override def toString = s"return(${expr.getOrElse("null")})"
}

//I IZ <function name> [YR <expression1> [AN YR <expression2> [AN YR <expression3> ...]]] MKAY
class FunctionCallPT(val name: IdentPT, val args: List[ExpressionPT]) extends ExpressionPT{
  override def toString = s"$name(${args mkString(",")})"
}
