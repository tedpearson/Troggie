package com.tedpearson.troggie
import org.scalaquery.session.Session
import akka.actor.Actor
import akka.actor.Props
import org.scalaquery.ql.basic.BasicTable
import org.scalaquery.meta.MTable
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.Query
import akka.pattern.ask
import akka.util.Timeout
import akka.dispatch.Await

class Factoid(conf: PluginConf) extends Plugin(conf) {
  implicit val session = conf.session
  val db = context.actorOf(Props(new FactoidDb))
  db ! Setup
  //testing
//  db ! Update("FoO","bsdfsdfar","is")
//  db ! Concat("foo"," as if")
//  implicit val timeout = Timeout(5000L)
//  println(Await.result(db ? MaxId, timeout.duration))
  
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
  case class Concat(key: String, also: String)
  case object Count
  case class FindById(id: Int)
  case object MaxId
  
  class FactoidDb(implicit session: Session) extends Actor {
    def receive = {
      case Setup => createIfNotExists(IsTable)
      case Update(key, value, isAre) => {
        val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i.edit
        if(row.firstOption.isEmpty) IsTable.all insert (key, value, isAre)
        else row.update((value, isAre))
      }
      case Find(key) => {
        val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i.all
        sender ! row.firstOption
      }
      case Delete(key) => {
        val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i
        row.delete
      }
      case Concat(key, also) => {
        val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i.value
        row.firstOption match {
          case Some(s) => row.update(s ++ also) 
          case None =>
        }
      }
      case Count => 
        sender ! Query(IsTable.count).first
      case FindById(id) => {
        val row = for(i <- IsTable if i.id === id) yield i.all
        sender ! row.firstOption
      }
      case MaxId => {
        sender ! Query(IsTable.id.max).first
      }
      case _ => println("Unsupported msg")
    }
  }
  
  object IsTable extends BasicTable[(Int, String, String, String)]("TROGGIE_IS") {
    def id = column[Int]("id", O PrimaryKey)
    def key = column[String]("is_key", O DBType("varchar(20)"))
    def value = column[String]("is_value", O DBType("varchar(500)"))
    def are = column[String]("is_are", O DBType("varchar(3)"))
    def * = id ~ key ~ value ~ are
    def all = key ~ value ~ are
    def edit = value ~ are
  }
}