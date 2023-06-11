package example

import scala.util.parsing.combinator._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

abstract class Term {
  def isPeanoNumber: Boolean
  def peanoNumber2Int: Int
  def isArray: Boolean
  def showArray: String
}

case class Var(name: String) extends Term {
  override def toString: String = name
  override def isPeanoNumber: Boolean = false
  override def peanoNumber2Int: Int = throw new Exception(
    "Var is not a Peano number"
  )
  override def isArray: Boolean = false
  override def showArray: String = s"Var($name)"
}

case class Con(name: String) extends Term {
  override def toString: String = name
  override def isPeanoNumber: Boolean = name == "0"
  override def peanoNumber2Int: Int =
    if (isPeanoNumber) 0 else throw new Exception("Con is not a Peano number")
  override def isArray: Boolean = name == "nil"
  override def showArray: String = if (isArray) "" else name
}

case class Ap(t1: Term, t2: Term) extends Term {
  override def toString: String = {
    if (this.isPeanoNumber) this.peanoNumber2Int.toString
    else if (this.isArray) "[" + this.showArray + "]"
    else s"(${t1.toString} ${t2.toString})"
  }

  override def isPeanoNumber: Boolean = t1 match {
    case Con(name) => name == "s" && t2.isPeanoNumber
    case _         => false
  }

  override def peanoNumber2Int: Int = t2.peanoNumber2Int + 1

  override def isArray: Boolean = {
    t1 match {
      case Ap(Con("cons"), x) => {
        if (t2 == Ap(Con("cons"), Con("nil"))) true
        else if (t2 == Con("nil")) true
        else t2.isArray
      }
      case _ => false
    }
  }

  override def showArray: String = {
    (t1, t2) match {
      case (Ap(Con("cons"), x), Con("nil")) => x.toString
      case (Ap(Con("cons"), x), y)          => x.toString + "," + y.showArray
      case _ => throw new Exception(s"not an array: ${this}")
    }
  }
}

abstract class MarkedTerm
case class MarkedApp(t1: MarkedTerm, t2: MarkedTerm) extends MarkedTerm
case class MarkedCon(name: String) extends MarkedTerm
case class NF(t: Term) extends MarkedTerm

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

  val el = Con("nil")
  val cons = Con("cons")
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
      fs.foldRight[Term](Term.el) { (f1, f2) =>
        Ap(Ap(Term.cons, f1), f2)
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
      rep1sep(parseSimpleTerm, keyword(":")) ^^ { case fs =>
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
    // parseArray2
    parseSimpleTerm
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

object Rewriter {

  val logger = Logger(LoggerFactory.getLogger("rewriter"))

  implicit class SubstitutionOps(s: TermRewriting.Substitution) {
    def lookUp(x: String): Option[Term] = {
      s.find { case (y, _) => x == y }.map { case (_, t) => t }
    }
  }

  /** Find a substitution that makes left term equal to target term
    * @param left
    *   term in the left hand side of a rule
    * @param targetTerm
    *   term to be checked if it matches with the left term
    */
  def findSubstitution(
      left: Term,
      targetTerm: Term
  ): Option[TermRewriting.Substitution] = {
    def inner(
        s: TermRewriting.Substitution,
        src: Seq[(Term, Term)]
    ): Option[TermRewriting.Substitution] = {
      logger.debug(s"inner: $s, $src")
      src match {
        case a if a.isEmpty => Some(s)
        case (Var(x), t) +: xs => {
          s.lookUp(x) match {
            case Some(t1) if (t1 == t) => inner(s, xs)
            case Some(_)               => None
            case None                  => inner((x, t) +: s, xs)
          }
        }
        case (Con(x), Con(y)) +: xs if x == y => inner(s, xs)
        case (Con(x), _) +: xs                => None
        case (Ap(t1, t2), Ap(t3, t4)) +: xs =>
          inner(s, (t1, t3) +: (t2, t4) +: xs)
        case _ => None
      }
    }

    inner(Seq(), Seq((left, targetTerm)))
  }

  /** Replace variables in term with terms in substitution
    * @param term
    *   term to be rewritten to marked term
    * @param s
    *   substitution to be used for conversion
    */
  def substitute(term: Term, s: TermRewriting.Substitution): MarkedTerm = {
    term match {
      case Var(x) =>
        s.lookUp(x) match {
          case Some(t) => NF(t)
          case None    => NF(term)
        }
      case Con(x)     => NF(term)
      case Ap(t1, t2) => MarkedApp(substitute(t1, s), substitute(t2, s))
    }
  }

  /** Rewrite root of term
    * @param t
    *   term to be converted to normal form
    * @param trs
    *   rules to be used for conversion
    */
  def rewriteRoot(t: Term, trs: TermRewriting.TRS): MarkedTerm = {
    trs match {
      case Seq() => NF(t)
      case (l, r) +: xs => {
        logger.debug(s"rewriteRoot: $t, $l -> $r")
        findSubstitution(l, t) match {
          case Some(s) => logger.debug(s"found: $s"); substitute(r, s)
          case None    => logger.debug(s"not found"); rewriteRoot(t, xs)
        }
      }
    }
  }

  /** Convert to Normal Form from marked term
    * @param mt
    *   marked term to be converted to normal form
    * @param trs
    *   rules to be used for conversion
    */
  def markedTerm2normalForm(
      mt: MarkedTerm,
      trs: TermRewriting.TRS
  ): MarkedTerm = {
    mt match {
      case NF(t)                     => NF(t)
      case MarkedCon(t)              => rewriteRoot(Con(t), trs)
      case MarkedApp(NF(t1), NF(t2)) => rewriteRoot(Ap(t1, t2), trs)
      case MarkedApp(t1, t2) =>
        markedTerm2normalForm(
          MarkedApp(
            markedTerm2normalForm(t1, trs),
            markedTerm2normalForm(t2, trs)
          ),
          trs
        )
    }
  }

  /** Convert to Normal Form
    * @param t
    *   term to be converted to normal form
    * @param trs
    *   rules to be used for conversion
    */
  def toNormalForm(t: Term, trs: TermRewriting.TRS): Term = {
    def inner(trs: TermRewriting.TRS, mt: MarkedTerm): Term = {
      mt match {
        case NF(t) => t
        case _     => inner(trs, markedTerm2normalForm(mt, trs))
      }
    }
    inner(trs, rewriteRoot(t, trs))
  }
}

object TermRewritingSystem {
  val logger = Logger(LoggerFactory.getLogger("trs"))
  def run(program: String): Either[Exception, Term] = {
    TermParser.parse(program) match {
      case TermParser.Success(trs, _) =>
        logger.debug(s"trs: $trs");
        Right(Rewriter.toNormalForm(Con("main"), trs))
      case TermParser.NoSuccess(msg, _) => Left(new Exception(msg))
    }
  }
}

object Main extends App {
  val file = scala.io.Source.fromFile(args(0))
  TermRewritingSystem.run(file.mkString) match {
    case Right(t) => println(t)
    case Left(e)  => println(e)
  }
}
