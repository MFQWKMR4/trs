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

  "array" should "be pretty-printed" in {
    // format: off
    val one = Term(1)
    val two = Term(2)
    val three = Term(3)
    val a = Ap(Ap(Con("cons"), one), Ap(Ap(Con("cons"), two), Ap(Ap(Con("cons"), three), Con("nil"))))
    val b = Ap(Con("cons"), Ap(one, Ap(Con("cons"), Ap(two, Ap(Con("cons"), Ap(three, Con("nil")))))))
    // format: on
    a.isArray shouldEqual true
    b.isArray shouldEqual false
  }

  "array" should "be parsable1" in {
    // format: off
    val one = Term(1)
    val two = Term(2)
    val three = Term(3)
    val a = Ap(Ap(Con("cons"), one), Ap(Ap(Con("cons"), two), Ap(Ap(Con("cons"), three), Con("nil"))))
    // format: on
    TermParser.parse("main -> [1,2,3] ; ") match {
      case TermParser.Success(result, _) =>
        println(result);
        result.head shouldEqual (Con("main"), a)
      case _ => fail()
    }
  }

  "append" should "be parsable" in {
    // format: off
    val one = Term(1)
    val two = Term(2)
    val three = Term(3)
    val four = Term(4)
    val five = Term(5)
    val onetwothree = Ap(Ap(Con("cons"), one), Ap(Ap(Con("cons"), two), Ap(Ap(Con("cons"), three), Con("nil"))))
    val fourfive = Ap(Ap(Con("cons"), four), Ap(Ap(Con("cons"), five), Con("nil")))
    val expected = Ap(Ap(Con("append"), onetwothree), fourfive)

    // format: on
    TermParser.parse("main -> append [1,2,3] [4,5]; ") match {
      case TermParser.Success(result, _) =>
        println(result);
        result.head shouldEqual (Con("main"), expected)
      case _ => fail()
    }
  }

}
