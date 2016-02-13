package co.uk.hackthetower.exercises

import cats.data.Xor
import co.uk.hackthetower.commands.server.Goodbye
import org.scalatest.{FlatSpec, Matchers}


class Ex2BotLogicSpec extends FlatSpec with Matchers {

  "processServerCommand" should "return Left if Goodbye is received with a large negative number as we can't lose, obviously it's a mistake!" in {
    val input = Xor.right(Goodbye(-10000))
//    Ex2BotLogic.processServerCommand(input).isLeft should be(true)
  }

  //TODO: add tests for your logic!
}
