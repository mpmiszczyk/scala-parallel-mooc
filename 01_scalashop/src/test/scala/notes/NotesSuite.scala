package notes

import org.scalatest._


class NotesSuite extends FunSuite with Matchers{
  import notes.MonteCarlo._

  test("monte carlo pi should calcualte more or less Pi") {
    monteCarloPiSeq(100000) should equal ( math.Pi +- 0.01 )
  }

}
