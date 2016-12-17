import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.matching.Regex

trait LolTokens extends StdTokens { 
 case class EolLit(chars: String) extends Token {
    override def toString = "EOL"
  }

}

class LolLexical extends StdLexical with LolTokens {
  
  override def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh && ch != '\n')
  
  override def whitespace: Parser[Any] = rep(
     whitespaceChar 
     | 'B' ~ 'T' ~ 'W' ~ rep(chrExcept(EofCh,'\n'))
      | 'O' ~ 'B' ~ 'T' ~ 'W' ~ comment) 
    
    override protected def comment: Parser[Any] = (
    'T' ~ 'L' ~'D' ~'R' ^^ { case _ => ' ' }
    | chrExcept(EofCh) ~ comment)
  
  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = 
      r.findPrefixMatchOf(in.source.subSequence(in.offset, in.source.length)) match {
        case Some(matched) =>
          Success(in.source.subSequence(in.offset,
            in.offset + matched.end).toString, in.drop(matched.end))
        case None => Failure(s"string matching regex `$r' expected but ${in.first} found", in)
      }
  }

  reserved ++= List("HAI", "IT", "VISIBLE", "R", "AN", "YARN", "NUMBR", "NUMBAR", "TROOF", "WIN", "FAIL", "NOOB", "GIMMEH", "ITZ", "SMOOSH", "MKAY", "NOT", "DIFFRINT", "MAEK", "MEBBE", "OIC", "GTFO", "UPPIN", "NERFIN", "YR", "TIL", "WILE", "KTHXBYE")
 
  override def token: Parser[Token] = {
    regex("[,\\s]+".r) ^^ {EolLit(_)} | 
    regex("I HAS A|IS NOW A|(SUM |DIFF |PRODUKT |QUOSHUNT |MOD |BIGGR |SMALLR |BOTH |EITHER |WON |ANY |ALL )OF|BOTH SAEM|O RLY\\?|YA RLY|NO WAI|OMG(WTF)?|WTF\\?|IM (IN|OUTTA) YR|HOW IZ I|FOUND YR|IF U SAY SO|I IZ".r) ^^  { Keyword(_) } |
    regex("[a-zA-Z][a-zA-Z0-9_]*".r) ^^ { processIdent(_) } |
    regex("\"((:\")|([^\"]))*\"".r) ^^ { StringLit(_) } |
    regex("[+-]?([0-9]*[.])?[0-9]+".r) ^^ { NumericLit(_) } 
  }
  
    

}
