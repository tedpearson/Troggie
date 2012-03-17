package com.tedpearson.troggie
import org.scalaquery.session.Session
import akka.actor.Actor
import akka.actor.Props
import org.scalaquery.ql.basic.BasicTable
import org.scalaquery.meta.MTable
import org.scalaquery.ql.basic.BasicDriver.Implicit._

class Factoid(conf: PluginConf) extends Plugin(conf) {
  implicit val session = conf.session
  val db = context.actorOf(Props(new FactoidDb))
  db ! Setup
  
  override protected def getStatusString = ""
  
  protected def processMessage(message: IrcMessage): Unit = message match {
    case m: PublicMessage =>
    case m: PrivateMessage =>
    case m: Action =>
    case _ =>
  }
  
  case object Setup
  case class Update(key: String, value: String, isAre: String)
  case class Find(key: String)
  case class Delete(key: String)
  case class Concat(also: String, key: String)
  case object Count
  case class FindById(id: Int)
  case object MaxId
  
  class FactoidDb(implicit session: Session) extends Actor {
    def receive = {
      case Setup => createIfNotExists(IsTable)
      case Update(key, value, isAre) =>
      case Find(key) =>
      case Delete(key) =>
      case Concat(also, key) =>
      case Count =>
      case FindById(id) =>
      case MaxId =>
      case _ => println("Unsupported msg")
    }
  }
  
  object IsTable extends BasicTable[(Int, String, String, String)]("TROGGIE_IS") {
    def id = column[Int]("id", O PrimaryKey)
    def is_key = column[String]("is_key", O DBType("varchar(20)"))
    def is_value = column[String]("is_value", O DBType("varchar(500)"))
    def is_are = column[String]("is_are", O DBType("varchar(3)"))
    def * = id ~ is_key ~ is_value ~ is_are
  }
}