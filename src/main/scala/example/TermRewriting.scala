package example

import scala.util.parsing.combinator._

abstract class Term
case class Var(name: String) extends Term
case class Con(name: String) extends Term
case class Ap(t1: Term, t2: Term) extends Term

abstract class MarkedTerm
case class MarkedApp(t1: MarkedTerm, t2: MarkedTerm) extends MarkedTerm
case class MarkedCon(name: String) extends MarkedTerm
case class NF(t: MarkedTerm) extends MarkedTerm

object TermRewriting {
  type Rule = (Term, Term)
  type TRS = Seq[Rule]
  type Substitution = Seq[(String, Term)]

}

object Term {

  val zero = Con("0")
  val succ = Con("s")

  def apply(n: Int): Term = {
    if (n == 0) zero
    else Ap(succ, apply(n - 1))
  }

  val el = Con("[]")
  val cons = Con(":")
}

object TermParser extends JavaTokenParsers {

  implicit class Foldable[A](xs: Seq[A]) {

    def foldLeft1(f: (A, A) => A): A = {
      xs.tail.foldLeft(xs.head) { (x, y) => f(x, y) }
    }

    def foldRight1(f: (A, A) => A): A = {
      xs.init.foldRight(xs.last) { (x, y) => f(x, y) }
    }
  }

  def spaces: Parser[String] = """\s*""".r

  def keyword(str: String): Parser[String] = spaces ~> str <~ spaces

  def parseTerm: Parser[Term] = (
    parseSimpleTerm ~ rep(parseSimpleTerm) ^^ { case f ~ fs =>
      fs.foldLeft(f) { (f1, f2) => Ap(f1, f2) }
    }
  )

  def parseTerm2: Parser[Term] = (
    rep1sep(parseSimpleTerm, keyword(",")) ^^ { case fs =>
      fs.foldRight(Ap(Term.cons, Term.el)) { (f1, f2) =>
        Ap(Term.cons, Ap(f1, f2))
      }
    }
  )

  def parseNumber: Parser[Term] = wholeNumber ^^ { case n => Term(n.toInt) }

  def parseVariable: Parser[Var] = """[A-Z][a-z]*""".r ^^ { Var(_) }

  def parseConstant: Parser[Con] = """[a-z]+""".r ^^ { Con(_) }

  def parseParen[Term](p: Parser[Term]): Parser[Term] = "(" ~> p <~ ")"

  def parseArray[Term](p: Parser[Term]): Parser[Term] = "[" ~> p <~ "]"

  def parseArray2: Parser[Term] = {
    val genArray = (t1: Term, t2: Term) => Ap(Ap(Term.cons, t1), t2)
    (
      rep1sep(parseSimpleTerm, keyword(",")) ^^ { case fs =>
        fs.foldRight1(genArray)
      }
    )
  }

  def parseSimpleTerm: Parser[Term] = (
    parseNumber
      | parseVariable
      | parseConstant
      | parseParen(parseTerm)
      | parseArray(parseTerm2)
  )

  def parseE3a: Parser[Term] = (
    keyword("(") ~ parseE1 ~ keyword(")") ^^ { case _ ~ t ~ _ => t }
  )

  def parseE3: Parser[Term] = (
    parseArray2
      | parseSimpleTerm
      | parseE3a
  )

  def parseE2: Parser[Term] = (
    rep1(parseE3) ^^ { case fs =>
      fs.foldLeft1 { (f1, f2) => Ap(f1, f2) }
    }
  )

  def parseE1: Parser[Term] = {
    val add = (t1: Term, t2: Term) => Ap(Con("+"), Ap(t1, t2))
    (
      rep1sep(parseE2, keyword("+")) ^^ { case fs =>
        fs.foldLeft1(add)
      }
    )
  }

  def parseRule: Parser[TermRewriting.Rule] = (
    parseE1 ~ keyword("->") ~ parseE1 ~ keyword(";") ^^ {
      case t1 ~ _ ~ t2 ~ _ => (t1, t2)
    }
  )

  def parseTRS: Parser[TermRewriting.TRS] = rep(parseRule)

  def parse(input: String): ParseResult[TermRewriting.TRS] =
    parseAll(parseTRS, input)
}

object TermRewritingSystem extends App {
  val file = scala.io.Source.fromFile(args(0))
  TermParser.parse(file.mkString) match {
    case TermParser.Success(trs, _) => {
      println(trs)
    }
    case TermParser.NoSuccess(msg, _) => {
      println(msg)
    }
  }
}
