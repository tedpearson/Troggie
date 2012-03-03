package com.tedpearson.troggie
import java.sql.Timestamp
import scala.compat.Platform.currentTime
import org.scalaquery.meta.MTable
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.session.Session
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import org.scalaquery.ql.SimpleFunction
import org.scalaquery.ql.Column
import java.util.Date
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.dispatch.Await

class Seen(conf: PluginConf) extends Plugin(conf) {
  implicit val session: Session = conf.session
  val troggie = context.actorFor("..")
  println(troggie)
  var db = context.actorOf(Props(new SeenDb()))
  db ! Setup
  
  def processMessage(message: IrcMessage) {
    message match {
      case m: Join => db ! Update(m.sender, "joining the channel", Some(m.channel), None)
      case m: PublicMessage => {
        db ! Update(m.sender, "saying: '%s'" format m.msg, Some(m.channel), Some(m.msg))
        matchMessage(m)
      }
      case m: Action => {
        if(m.target.startsWith("#")) {
          val saying = "*%s %s" format (m.sender, m.action)
          db ! Update(m.sender, "saying: '%s'" format saying, Some(m.target), Some(saying))
        }
      }
      case m: Kick => {
        db ! Update(m.sender, "kicking %s off %s" format (m.rcpt, m.channel), Some(m.channel), None)
        db ! Update(m.rcpt, "being kicked off %s" format m.channel, Some(m.channel), None)
      }
      case m: NickChange => {
        db ! Update(m.oldNick, "changing their nick to '%s'" format m.newNick, None, None)
        db ! Update(m.newNick, "changint their nick from '%s'" format m.oldNick, None, None)
      }
      case m: Notice => {
        if(m.target.startsWith("#")) {
          db ! Update(m.sender, "making a notice: '%s'" format m.msg, Some(m.target), None)
        }
      }
      case m: Part => db ! Update(m.sender, "leaving the channel" format m.channel, Some(m.channel), None)
      case m: Quit => db ! Update(m.sender, "quitting (%s)" format m.msg, None, None)
      case m: Topic => db ! Update(m.setBy, "changing the topic to '%s'" format m.topic, Some(m.channel), None)
      case m: PrivateMessage => matchMessage(m)
      case _ =>
    }
  }
  
  val SeenQuery = """(?i)seen ([\S]+).*""".r
  def matchMessage(m: Message) {
      implicit val timeout = Timeout(5000L)
    println(m.msg)
    m.msg match {
      case SeenQuery(nick) => {
        db ? Find(nick) mapTo manifest[Saw] onSuccess {
          case Saw(nick, time, doing, channel, saying, saying_time) => {
            val doCount = m.isInstanceOf[PublicMessage]
            val target = if(doCount) m.asInstanceOf[PublicMessage].channel else m.sender
            val chan = if(channel.isDefined) "on %s " format channel.getOrElse("") else ""
            // TODO: format time
            troggie ! SendMessage(target, "%s was last seen %s%s ago, %s %s"
                .format (nick, chan, time, doing, saying_time), doCount)
            if(time != saying_time.get) {
              // this not equals doesn't appear to be working currently
              troggie ! SendMessage(target, "%s last spoke %s%s ago, %s %s"
                  .format (nick, chan, time, saying.get, saying_time), doCount)
            }
          }
        }
      }
    }
  }
  
  class SeenDb(implicit session: Session) extends Actor {
    val lower = SimpleFunction[String]("lower")
    def lower2(c: Column[String]) = lower(Seq(c))
    def receive = {
      case Setup => {
        if(MTable.getTables.firstOption.isEmpty) SeenTable.ddl.create
      }
      case Update(nick, doing, channel, saying) => {
        val row = for(s <- SeenTable if s.nick is nick)
          yield s.all
        val time = new Timestamp(currentTime)
        val sTime = if(saying.isDefined) Some(time) else None
        row.firstOption match {
          case Some(r) => {
            if(saying.isDefined) row.update((nick, time, doing, channel, saying, sTime))
            else row.update((nick, time, doing, channel, r._5, r._6))
          }
          case None => SeenTable.all insert (nick, time, doing, channel, saying, sTime)
        }
      }
      case Find(nick) => {
        val find = for(s <- SeenTable if lower2(s.nick) === nick) yield s.all
        find.firstOption match {
          case Some((nick, time, doing, channel, saying, saying_time)) => {
            sender ! Saw(nick, time, doing, channel, saying, saying_time)
          }
          case None =>
        }
      }
    }
  }
  
  case class Update(nick: String, doing: String, channel: Option[String], saying: Option[String])
  case class Find(nick: String)
  case class Saw(nick: String, time: Date, doing: String, channel: Option[String],
      saying: Option[String], saying_time: Option[Date])
  object Setup
  
  object SeenTable extends Table[(Int, String, Timestamp, String, Option[String], Option[String], Option[Timestamp])]("TROGGIE_SEEN") {
    def id = column[Int]("id", O PrimaryKey)
    def nick = column[String]("nick", O DBType("text"))
    def time = column[Timestamp]("time")
    def doing = column[String]("doing", O DBType("text"))
    def channel = column[Option[String]]("channel", O DBType("text"))
    def saying = column[Option[String]]("saying", O DBType("text"))
    def saying_time = column[Option[Timestamp]]("saying_time")
    def nick_index = index("nick_index", nick)
    def * = id ~ nick ~ time ~ doing ~ channel ~ saying ~ saying_time
    def all = nick ~ time ~ doing ~ channel ~ saying ~ saying_time
  }
}