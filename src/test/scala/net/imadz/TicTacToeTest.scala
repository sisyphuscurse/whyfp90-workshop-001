package net.imadz

import net.imadz.TicTacToeGame.ChessBoard.allWinningSolutions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TicTacToeTest extends AnyFlatSpec with should.Matchers {

  "The algorithm" should "generate all the winning solutions in 2" in {
    // given
    val rows, columns, win = 2
    // when
    val solutions = allWinningSolutions(rows, columns, win)
    solutions.should(contain(Set((0, 0), (0, 1))))
    solutions.should(contain(Set((0, 0), (1, 0))))
    solutions.should(contain(Set((0, 0), (1, 1))))
    solutions.should(contain(Set((0, 1), (1, 1))))
    solutions.should(contain(Set((0, 1), (1, 0))))
    solutions.should(contain(Set((1, 0), (1, 1))))
    solutions.foreach(println)
    solutions.size shouldBe (6)
  }

  "The algorithm" should "generate all the winning solutions in tic-tac-toe" in {
    // given
    val rows, columns, win = 3
    // when
    val solutions = allWinningSolutions(rows, columns, win)
    solutions.should(contain(Set((0, 0), (0, 1), (0, 2))))
    solutions.should(contain(Set((0, 0), (1, 0), (2, 0))))
    solutions.should(contain(Set((0, 0), (1, 1), (2, 2))))
    solutions.should(contain(Set((0, 1), (1, 1), (2, 1))))
    solutions.should(contain(Set((0, 2), (1, 1), (2, 0))))
    solutions.should(contain(Set((0, 2), (1, 2), (2, 2))))
    solutions.should(contain(Set((1, 0), (1, 1), (1, 2))))
    solutions.should(contain(Set((2, 0), (2, 1), (2, 2))))
    solutions.foreach(println)
    solutions.size shouldBe (8)
  }

}
