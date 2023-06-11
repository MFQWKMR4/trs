package example

import org.scalatest._

class TermRewritingSystemSpec extends FlatSpec with Matchers {
  "fact 3" should "be 6" in {

    val program = """
    0   + Y -> Y ;
    s X + Y -> s (X + Y) ;

    mul 0     Y -> 0 ;
    mul (s X) Y -> mul X Y + Y ;

    fact 0  -> s 0 ;
    fact (s X) -> mul (s X) (fact X) ;

    main -> fact 3 ;
    """

    // format: off
    TermRewritingSystem.run(program) match {
      case Right(result) =>{
        result shouldEqual Ap(Con("s"),Ap(Con("s"),Ap(Con("s"), Ap(Con("s"), Ap(Con("s"), Ap(Con("s"), Con("0")))))))
        result.toString shouldEqual "6"
    }
      case Left(_) => fail()
    }
    // format: on
  }

  "append array" should "work" in {

    // format: off
    val program = """
    append nil          Ys -> Ys ;
    append (cons X Xs) Ys -> cons X (append Xs Ys) ;
    main -> append [1,2,3] [4,5] ;
    """
    TermRewritingSystem.run(program) match {
      case Right(result) => {
        result.toString shouldEqual "[1,2,3,4,5]"
      }
      case Left(_) => fail()
    }
    // format: on
  }

}
