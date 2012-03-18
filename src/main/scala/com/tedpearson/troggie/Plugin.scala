package com.tedpearson.troggie
import java.util.Properties
import org.scalaquery.session.Session
import akka.actor._
import org.scalaquery.meta.MTable
import org.scalaquery.ql.basic.BasicTable
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.SimpleFunction
import org.scalaquery.ql.Column
import org.scalaquery.ql.SimpleExpression


abstract class Plugin(conf: PluginConf) extends Actor {
  val troggie = context.actorFor("..")
  protected def getStatusString: String
  val lower1 = SimpleFunction[String]("LOWER")
  def lowerc(c: Column[String]) = lower1(Seq(c))
  def lowers = SimpleExpression.unary[String, String] { (i, b, qb) =>
    b += "LOWER("
    qb.expr(i, b)
    b += ")"
  }
  def receive = {
    case m: GetStatus => sender ! Status(getStatusString)
    case m: IrcMessage => processMessage(m)
    case _ => // we don't handle other messages.
  }
  protected def processMessage(m: IrcMessage): Unit
  protected def createIfNotExists [T] (table: BasicTable[T])(implicit session: Session) {
    if(!MTable.getTables.list.map(_.name.name).contains(table.tableName)) {
      table.ddl.create
    }
  }
  protected def send(target: String, msg: String) {
    troggie ! SendMessage(target, msg)
  }
}

case class PluginConf(p: Properties, session: Session) {}