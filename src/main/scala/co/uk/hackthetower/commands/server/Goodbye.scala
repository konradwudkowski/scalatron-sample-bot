package co.uk.hackthetower.commands.server

/**
  * Goodbye(energy=int)
  *
  * "Goodbye" is the last command sent by the server to a plug-in after all other invocations.
  * The plug-in should use this opportunity to close any open files (such as those used for debug logging)
  * and to relinquish control of any other resources it may hold.
  *
  * Parameters:
  *
  * energy: the bot's final energy level
  */
case class Goodbye(energy: Int) extends ServerCommand

