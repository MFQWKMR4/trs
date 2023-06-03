package example

import org.scalatest._

class TermParserSpec extends FlatSpec with Matchers {
  "add rule" should "be interpreted as TRS.Rule" in {

    TermParser.parse("0   + Y -> Y ;") match {
      case TermParser.Success(result, _) =>
        result.head shouldEqual (
          Ap(
            Con("+"),
            Ap(Con("0"), Var("Y"))
          ),
          Var("Y")
        )
      case _ => fail()
    }

    TermParser.parse("s X + Y -> s (X + Y) ;") match {
      case TermParser.Success(result, _) =>
        result.head shouldEqual (Ap(
          Con("+"),
          Ap(Ap(Con("s"), Var("X")), Var("Y"))
        ), Ap(
          Con("s"),
          Ap(Con("+"), Ap(Var("X"), Var("Y")))
        ))
      case _ => fail()
    }
  }
}
