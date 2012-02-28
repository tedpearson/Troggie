package com.tedpearson.troggie
import java.util.Properties
import akka.actor._
import java.sql.Connection

abstract class Plugin(conf: PluginConf) extends Actor {
	def getStatusString = ""
	def receive = {
		case m: IrcMessage => processMessage(m)
		case _ => // we don't handle other messages.
	}
	protected def processMessage(m: IrcMessage): Unit
}

case class PluginConf(p: Properties, conn: Connection)