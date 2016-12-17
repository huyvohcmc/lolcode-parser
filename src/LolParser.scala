import scala.util.parsing.combinator.syntactical.StdTokenParsers

class LolParser extends StdTokenParsers {
  type Tokens = LolTokens
  val lexical = new LolLexical
  
  def parse(s: String) = phrase(program)(new lexical.Scanner(s))
  
  def eol: Parser[String] = elem("eol", _.isInstanceOf[lexical.EolLit]) ^^ (_.chars)

  def id: Parser[IdentPT] = (ident) ^^ {
    case a => new IdentPT(a)
  }
  
  def value:Parser[ValuePT] = (numericLit | stringLit) ^^ {
    case a => new ValuePT(a)
  }
  
  def program: Parser[ProgramPT] =  (opt(eol) ~> "HAI" ~> opt(numericLit) <~ eol) ~> rep(statement)  <~ "KTHXBYE" <~ opt(eol) ^^
  {
    case a => new ProgramPT(a)
  }
  
  def vardec: Parser[VariableDeclarationPT] = ("I HAS A" ~> ident) ~ opt("ITZ" ~> (expression)) ^^ {
    case a ~ b => new VariableDeclarationPT(new IdentPT(a),b)
  }
  
  def assignment:Parser[AssignmentPT] = (ident <~ "R") ~ expression ^^ {
    case a ~ b => new AssignmentPT(new IdentPT(a),b)
  }
  
  def input: Parser[PrintPT] = "VISIBLE" ~> rep(expression) ~ opt("!") ^^ 
  {
    case a ~ b => new PrintPT(a, b!=None)
  }
  
  def output: Parser[ReadPT] = "GIMMEH" ~> ident ^^ {
    a => new ReadPT(new IdentPT(a))
  }

  def break: Parser[BreakPT] = "GTFO" <~ eol ^^ {
    case a => new BreakPT()
  }
  
  def statement: Parser[StatementPT] = (vardec|assignment|input|output|expression|ifElseStatement|switch|loop|function) <~ eol ^^ {case a => a}
  
  def normalStatement: Parser[StatementPT] = (statement | break) ^^ {
    case a => a
  }

  // excluded break
  def specialStatement: Parser[StatementPT] = (statement | functionReturn) ^^ {
    case a => a
  }

  def expression: Parser[ExpressionPT] = (binaryOperator|unaryOperator|multiArityOperator|id|value|functionCall) ^^ {
    case a => a
  }
  
  def binaryOperator: Parser[BinaryOperatorPT] = (("SUM OF"|"DIFF OF"|"PRODUKT OF"|"QUOSHUNT OF"|"MOD OF"|"BIGGR OF"|"SMALLR OF"|"BOTH OF"|"EITHER OF"|"WON OF"|"BOTH SAEM"|"DIFFRINT") ~ expression) ~ (opt("AN") ~> expression) ^^ {
    case a ~ b ~ c => new BinaryOperatorPT(a,b,c)
  }

  def unaryOperator: Parser[UnaryOperatorPT] = "NOT" ~ expression ^^ {
    case a ~ b => new UnaryOperatorPT(a, b)
  }

  def multiArityOperator: Parser[MultiArityOperatorPT] = (("ALL OF"|"ANY OF") ~ rep1sep(expression, "AN") <~ "MKAY") ^^ {
    case a ~ b => new MultiArityOperatorPT(a, b)
  }
  

  /*********************** IF ELSE ********************************************/
  def elseIf: Parser[ElseIFPT] = ("MEBBE" ~> expression <~ eol) ~ rep(statement) ^^ {
    case a ~ b => new ElseIFPT(a, b)
  }

  def ifElse: Parser[IfElsePT] = "YA RLY" ~> eol ~> rep(statement) ~ opt(rep(elseIf)) ~ opt("NO WAI" ~> eol ~> rep(statement)) ^^ {
    case a ~ b ~ c => new IfElsePT(a, b, c)
  }

  def ifElseStatement: Parser[StatementPT] = (opt(eol) ~> "O RLY?" ~> eol ~> ifElse <~ "OIC") ^^ {
    case a => a
  }


  /*********************** CASE ***********************************************/
  def valueCase: Parser[ValueCasePT] = rep1("OMG" ~> value <~ eol) ~ rep(normalStatement) ^^ {
    case a ~ b => new ValueCasePT(a, b)
  }

  def defaultCase: Parser[DefaultCasePT] = "OMGWTF" ~> eol ~> rep(normalStatement) ^^ {
    case a => new DefaultCasePT(a)
  }

  def switch: Parser[SwitchPT] = "WTF?" ~> eol ~> rep1(valueCase) ~ opt(defaultCase) <~ "OIC" ^^ {
    case a ~ Some(b) => new SwitchPT(a :+ b)
    case a ~ None => new SwitchPT(a)
  }


  /*********************** LOOP ***********************************************/
  def loopCondition: Parser[LoopConditionPT] = (("UPPIN"|"NERFIN"|ident) <~ "YR") ~ id ~ opt(("TIL"|"WILE") ~ expression) ^^ {
    case a ~ b ~ Some(c ~ d) => new LoopConditionPT(a, b, Option(c -> d))
    case a ~ b ~ None => new LoopConditionPT(a, b, Option("None" -> new IdentPT("-1")))
  }

  def loop: Parser[LoopPT] = ("IM IN YR" ~> id ~ opt(loopCondition) <~ eol) ~ rep(normalStatement) <~ "IM OUTTA YR" <~ id ^^ {
    case a ~ b ~ c => new LoopPT(a, b, c)
  }


  /*********************** FUNCTION *******************************************/
  def function: Parser[FunctionPT] = ("HOW IZ I" ~> id ~ repsep("YR" ~> id, "AN") <~ eol) ~ rep(specialStatement) <~ "IF U SAY SO" ^^ {
    case a ~ b ~ c => new FunctionPT(a, b, c)
  }

  def functionReturn: Parser[FunctionReturnPT] = opt("FOUND YR" ~> expression) ~ opt("GTFO") <~ eol ^^ {
    case a ~ b => new FunctionReturnPT(a)
  }

  def functionCall: Parser[FunctionCallPT] = "I IZ" ~> id ~ repsep("YR" ~> expression, "AN") <~ "MKAY" ^^ {
    case a ~ b => new FunctionCallPT(a, b)
  }
}
