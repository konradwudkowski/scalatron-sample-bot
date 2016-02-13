package co.uk.hackthetower.exercises

import cats.Traverse.ops.toAllTraverseOps
import cats.data.Validated.{Invalid, Valid, invalid, valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.std.list._
import cats.syntax.option._
//import cats.syntax.apply._
import cats.syntax.cartesian._
import co.uk.hackthetower.commands.server.{Goodbye, React, ServerCommand, Welcome}

import scala.util.Try

/**
  * First exercise: Implement method 'parseInput'.
  *
  * This method must validate the input received from the server and return an object indicating
  * if the input was correct or not.
  * As per specification the input, if valid, will contain a single command, and will match one of the
  * 'ServerCommands' defined in the codebase. Incorrect scenarios may include invalid commands,
  * multiple commands, etc.
  *
  * Aims:
  * - Learn to work with Validated and OneAnd instances (NonEmptyList in this case)
  * - Understand the differences between Validated and Xor
  * - Understand the differences between OneAnd and NonEmptyList
  *
  * See:
  * - http://typelevel.org/cats/tut/validated.html
  * - http://typelevel.org/cats/tut/oneand.html
  */
object Ex1ValidateInput {

  //TODO: START HERE
  /**
    * This method parses the input of the server and validates it to ensure we got a valid command
    *
    * @param input the input sent by the server. As per specification it will only have 1 command.
    * @return a ValidatedNel[String, ServerCommand], equivalent to Validated[NonEmptyList[String], ServerCommand]
    */
  def parseInput(input: String): ValidatedNel[String, ServerCommand] = {

    extractParams(input).map(_.toMap) match {
      case Valid(data) => {
        input match {
          case i if i.startsWith(MsgPrefixes.GOODBYE) => goodbyeParser(data)
          case i if i.startsWith(MsgPrefixes.WELCOME) => welcomeParser(data)
          case i if i.startsWith(MsgPrefixes.REACT) => reactParser(data)
          case _ => Invalid(NonEmptyList("unknown message type"))
        }
      }
      case invalidData => invalid(invalidData.toEither.left.get)
    }

  }

  def extractParams(input: String): ValidatedNel[String, List[(String,String)]] = {
    val PARAMS_REGEX = """.+\((.+)\)""".r
    input match {
      case PARAMS_REGEX(params) => params.split(",").toList.traverseU(convertKeyValueToATuple)
      case _ => invalid(NonEmptyList("unable to extract params from: " + input))
    }
  }

  def convertKeyValueToATuple(keyValue: String) = keyValue.split("=").toList match {
    case first :: second :: Nil => valid((first,second))
    case _ => invalid(NonEmptyList(s"the number of equal signs was not correct in $keyValue"))
  }

  object MsgPrefixes {
    val GOODBYE = "Goodbye"
    val REACT = "React"
    val WELCOME = "Welcome"
  }

  def parseInt(s: String) = Try { s.toInt }.toOption

  def parseTupleOfInts(s: String): Option[(Int,Int)] = Try {
    s.split(":").toList.flatMap(parseInt) match {
      case int1 :: int2 :: Nil => (int1,int2)
    }
  }.toOption

  def goodbyeParser(data: Map[String,String]): ValidatedNel[String, Goodbye] = {
    val maybeGoodbyeParser = data.get("energy").flatMap(parseInt).map(Goodbye.apply)
    maybeGoodbyeParser.map(valid).getOrElse(invalid(NonEmptyList("error parsing React")))
  }

  def welcomeParser(data: Map[String,String]): ValidatedNel[String, Welcome] = {
    val maybeWelcomeParser = for {
      name        <- data.get("name")
      apocalypse  <- data.get("apocalypse").flatMap(parseInt)
      round       <- data.get("round").flatMap(parseInt)
      maxslaves   <- data.get("maxslaves").flatMap(parseInt)
    } yield Welcome(name,apocalypse,round,maxslaves)
    maybeWelcomeParser.map(valid).getOrElse(invalid(NonEmptyList("error parsing React")))
  }

  val x: Validated[String, String] = Option("konrad").toValid("unable to extract generation")
  val y: Validated[String, String] = Option("konrad").toValid("unable to extract generation")




  def reactParser(data: Map[String,String]): ValidatedNel[String, React] = {

//    val x: Validated[NonEmptyList[String], Int] = data.get("generation").flatMap(parseInt).toValid(NonEmptyList("error getting generation"))
//    val y: Validated[NonEmptyList[String], String] = data.get("name").toValid(NonEmptyList("error getting name"))
//
//    (x |@| y) map { _ + _ }
//
//    Validated.fromOption(data.get("generation").flatMap(parseInt), NonEmptyList("problem with generation"))

    val maybeReactParser = for {
      generation    <-  data.get("generation").flatMap(parseInt)
      name          <-  data.get("name")
      time          <-  data.get("time").flatMap(parseInt)
      view          <-  data.get("view")
      energy        <-  data.get("energy")
      master        =  data.get("master").flatMap(parseTupleOfInts) // at the moment no distinction between failing and absence
      collision     =  data.get("collision").flatMap(parseTupleOfInts)
      slaves        <-  data.get("slaves").flatMap(parseInt)
      states        <-  Option(Map[String,String]())
    } yield React(generation, name, time, view, energy, master, collision, slaves, states)
    maybeReactParser.map(valid).getOrElse(invalid(NonEmptyList("error parsing React")))
  }






}


