package com.tedpearson.troggie
import java.util.Properties
import org.scalaquery.session.Session
import akka.actor._
import org.scalaquery.meta.MTable
import org.scalaquery.ql.basic.BasicTable
import org.scalaquery.ql.basic.BasicDriver.Implicit._

abstract class Plugin(conf: PluginConf) extends Actor {
  def getStatusString = ""
  def receive = {
    case m: IrcMessage => processMessage(m)
    case _ => // we don't handle other messages.
  }
  protected def processMessage(m: IrcMessage): Unit
  protected def createIfNotExists [T] (table: BasicTable[T])(implicit session: Session) {
    if(!MTable.getTables.list.map(_.name.name).contains(table.tableName)) {
      table.ddl.create
    }
  }
}

case class PluginConf(p: Properties, session: Session) {}