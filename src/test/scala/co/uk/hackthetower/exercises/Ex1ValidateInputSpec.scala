package co.uk.hackthetower.exercises

import cats.data.Validated.{Invalid, Valid}
import cats.data.NonEmptyList
import co.uk.hackthetower.commands.server.Welcome
import org.scalatest.{Matchers, FlatSpec}

import scala.util.Random


class Ex1ValidateInputSpec extends FlatSpec with Matchers {

  "function extractParams" should "extract a list of param pairs" in {
    Ex1ValidateInput.extractParams("Game(foo=bar)") shouldBe Valid(List("foo" -> "bar"))
    Ex1ValidateInput.extractParams("Foo(foo=bar,abc=xyz)") shouldBe Valid(List("foo" -> "bar", "abc" -> "xyz"))
  }

  "function extractParams" should "return Invalid with explanation what went wrong" in {
    Ex1ValidateInput.extractParams("Game(foo=bar=baz)") shouldBe Invalid(
      NonEmptyList("the number of equal signs was not correct in foo=bar=baz"))
  }

  "parseInput" should "return Valid if input contains a valid server command" in {
    val input = "Goodbye(energy=-1)"
    Ex1ValidateInput.parseInput(input).isValid should be(true)
  }

  it should "return Invalid if input is empty" in {
    val input = ""
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

  it should "return Invalid if input is nonsensical garbage" in {
    val input = Random.nextString(40)
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

  it should "return Invalid if input doesn't match a server command" in {
    val input = "Log(text=this is a bot command not a server command!)"
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

  it should "return Invalid if input contains multiple server commands" in {
    val input = "Goodbye(1)|Goodbye(4)"
    Ex1ValidateInput.parseInput(input).isInvalid should be(true)
  }

  it should "parse a welcome msg successfully" in {
    val input = "Welcome(name=name,apocalypse=1000,round=1,maxslaves=5)"
    Ex1ValidateInput.parseInput(input) shouldBe Valid(Welcome("name", 1000, 1, 5))
  }

  it should "parse a React msg successfully" in {
    val input = "React(generation=1,name=name,time=1,view=view,energy=view,master=5:5,collision=10:10,slaves=99)"
//    Ex1ValidateInput.parseInput(input) shouldBe Valid(Welcome("name", 1000, 1, 5))
  }


}
