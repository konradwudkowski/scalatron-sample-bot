package co.uk.hackthetower.exercises

import cats.data.Xor
import co.uk.hackthetower.commands.bot._
import co.uk.hackthetower.commands.server.ServerCommand
import cats.data.Xor.{Left,Right}
import co.uk.hackthetower.commands.server._



/**
  * Second exercise: Implement method 'processServerCommand'
  *
  * This method receives an Xor[String, ServerCommand] instance and will return an Xor answer.
  *
  * Input:
  * - we receive a Left of String if the input is invalid
  * - we receive a Right of ServerCommand if we got the right command
  *
  * Note that receiving a Left doesn't mean we can't still send BotCommands to the server with instructions.
  *
  * Output:
  * - if the processing fails, we will send a Left with an error message.
  * This will be automatically converted to Say and Log commands for the server.
  * - if the processing succeeds, we will send a Right with a list of BotCommands to send to the server
  *
  * Aims:
  * - Learn to work with Xor to propagate error states
  * - Learn to use both right/left sides as well as mapping over them
  *
  * See:
  * - http://typelevel.org/cats/tut/xor.html
  */
object Ex2BotLogic {

  def processServerCommand(command: Xor[String, ServerCommand]): Xor[String, List[BotCommands]] = {
    command match {
      case Left(invalid) => Left("wrong command")
      case Right(cmd) => cmd match {
        case Goodbye(energy) =>  Right(List(Log(s"I have $energy of energy")))
        case Welcome(name, apocalypse, round, maxslaves) => Right(List(Log("Welcome to you too")))
        case React(generation,name,time,view,energy,master,collision,slaves,state) =>
          Right(List(Move(1,1)))
      }
    }
  }

}
