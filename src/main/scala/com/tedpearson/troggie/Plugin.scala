package com.tedpearson.troggie
import java.util.Properties

import org.scalaquery.session.Session

import akka.actor._

abstract class Plugin(conf: PluginConf) extends Actor {
  def getStatusString = ""
  def receive = {
    case m: IrcMessage => processMessage(m)
    case _ => // we don't handle other messages.
  }
  protected def processMessage(m: IrcMessage): Unit
}

case class PluginConf(p: Properties, session: Session) {}