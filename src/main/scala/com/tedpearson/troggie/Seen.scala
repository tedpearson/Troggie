package com.tedpearson.troggie
import java.sql.Timestamp
import java.util.Date
import scala.compat.Platform.currentTime
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import org.scalaquery.meta.MTable
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.ql.Column
import org.scalaquery.ql.SimpleFunction
import org.scalaquery.session.Session
import akka.actor.{Props, Actor}
import akka.pattern.ask
import akka.util.Timeout

class Seen(conf: PluginConf) extends Plugin(conf) {
  implicit val session: Session = conf.session
  var db = context.actorOf(Props(new SeenDb()))
  val formatter = DateTimeFormat.forPattern("[EEE MMM d, HH:mm:ss yyyy z]")
  db ! Setup
  
  override protected def getStatusString = ""
  
  def processMessage(message: IrcMessage) = message match {
    case m: Join => db ! Update(m.sender, "joining the channel", Some(m.channel), false)
    case m: PublicMessage => {
      matchMessage(m)
      val msg = "saying: '%s'" format m.msg
      db ! Update(m.sender, msg, Some(m.channel), true)
    }
    case m: Action => {
      if(m.target.startsWith("#")) {
        val msg = "saying: '*%s %s'" format(m.sender, m.action)
        db ! Update(m.sender, msg, Some(m.target), true)
      }
    }
    case m: Kick => {
      db ! Update(m.sender, "kicking %s off %s" format (m.rcpt, m.channel), Some(m.channel), false)
      db ! Update(m.rcpt, "being kicked off %s" format m.channel, Some(m.channel), false)
    }
    case m: NickChange => {
      db ! Update(m.sender, "changing their nick to '%s'" format m.newNick, None, false)
      db ! Update(m.newNick, "changing their nick from '%s'" format m.sender, None, false)
    }
    case m: Notice => {
      if(m.target.startsWith("#")) {
        db ! Update(m.sender, "making a notice: '%s'" format m.msg, Some(m.target), false)
      }
    }
    case m: Part => db ! Update(m.sender, "leaving the channel" format m.channel, Some(m.channel), false)
    case m: Quit => db ! Update(m.sender, "quitting (%s)" format m.msg, None, false)
    case m: Topic => db ! Update(m.sender, "changing the topic to '%s'" format m.topic, Some(m.channel), false)
    case m: PrivateMessage => matchMessage(m)
    case _ =>
  }
  
  val SeenQuery = """(?i)seen ([\S]+).*""".r
  def matchMessage(m: Message) {
      implicit val timeout = Timeout(5000L)
    m.msg match {
      case SeenQuery(nick) => {
        db ? Find(nick) mapTo manifest[Saw] onSuccess {
          case Saw(nick, time, doing, channel, saying, saying_time) => {
            val doCount = m.isInstanceOf[PublicMessage]
            val target = if(doCount) m.asInstanceOf[PublicMessage].channel else m.sender
            val chan = if(channel.isDefined) "on %s " format channel.getOrElse("") else ""
            val since = Utils.formatSince(time.getTime, currentTime)
            // TODO: format time
            val niceTime = formatter.print(new DateTime(time))
            troggie ! SendMessage(target, "%s was last seen %s%s ago, %s %s"
                .format (nick, chan, since, doing, niceTime), doCount)
                println("%s%s" format (saying_time, time))
            if(saying_time exists {_ != time}) {
              val niceSaying = formatter.print(new DateTime(saying_time.get))
              val since = Utils.formatSince(saying_time.get.getTime, currentTime)
              troggie ! SendMessage(target, "%s last spoke %s%s ago, %s %s"
                  .format (nick, chan, since, saying.get, niceSaying), doCount)
            }
          }
        }
      }
      case _ =>
    }
  }
  
  class SeenDb(implicit session: Session) extends Actor {
    def receive = {
      case Setup => createIfNotExists(SeenTable)
      case Update(nick, doing, channel, isSaying) => {
        val row = for(s <- SeenTable if s.nick is nick) yield s.all
        val time = new Timestamp(currentTime)
        val sTime = if(isSaying) Some(time) else None
        val saying = if(isSaying) Some(doing) else None
        row.firstOption match {
          case Some(r) => {
            if(isSaying) row.update((nick, time, doing, channel, saying, sTime))
            else row.update((nick, time, doing, channel, r._5, r._6))
          }
          case None => SeenTable.all insert (nick, time, doing, channel, saying, sTime)
        }
      }
      case Find(nick) => {
        val find = for(s <- SeenTable if lowerc(s.nick) === lowers(nick)) yield s.all
        find.firstOption match {
          case Some((nick, time, doing, channel, saying, saying_time)) => {
            sender ! Saw(nick, time, doing, channel, saying, saying_time)
          }
          case None =>
        }
      }
    }
  }
  
  case class Update(nick: String, doing: String, channel: Option[String], saying: Boolean)
  case class Find(nick: String)
  case class Saw(nick: String, time: Date, doing: String, channel: Option[String],
      saying: Option[String], saying_time: Option[Date])
  case object Setup
  
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