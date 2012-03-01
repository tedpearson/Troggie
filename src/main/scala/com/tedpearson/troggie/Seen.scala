package com.tedpearson.troggie
import java.sql.Timestamp

import scala.compat.Platform.currentTime

import org.scalaquery.meta.MTable
import org.scalaquery.ql.TypeMapper._
import  org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.session.Session

      
class Seen(conf: PluginConf) extends Plugin(conf) {
  implicit val session: Session = conf.session
  setupDb
  
  def processMessage(message: IrcMessage) {
    message match {
      case m: Join => updateDb(m.sender, "joining the channel", m.channel)
      case m: Message => updateDb(m.sender, "saying: '%s'" format m.msg, m.channel, saying = Some(m.msg))
      case m: Action => {
        if(m.target.startsWith("#")) {
          val saying = "*%s %s" format (m.sender, m.action)
          updateDb(m.sender, "saying: '%s'" format saying, m.action, saying = Some(saying));
        }
      }
      case _ =>
    }
  }
  
  def setupDb {
    if(MTable.getTables.firstOption.isEmpty) SeenTable.ddl.create
    for(s <- SeenTable if s.nick === "foo") yield s
  }
  

  def updateDb(nick: String, doing: String, channel: String, saying: Option[String] = None) {
    val row = for(s <- SeenTable if s.nick is nick)
      yield s.nick ~ s.time ~ s.doing ~ s.channel ~ s.saying ~ s.saying_time
    val time = new Timestamp(currentTime)
    val sTime = if(saying.isDefined) Some(time) else None
    row.firstOption match {
      case Some(r) => {
        if(saying.isDefined) row.update((nick, time, doing, channel, saying, sTime))
        else row.update((nick, time, doing, channel, r._5, r._6))
      }
      case None => {
        SeenTable.all insert {
          (nick, time, doing, channel, saying, sTime)
        }
      }
    }
  }
}


object SeenTable extends Table[(Int, String, Timestamp, String, String, Option[String], Option[Timestamp])]("TROGGIE_SEEN") {
  def id = column[Int]("id", O PrimaryKey)
  def nick = column[String]("nick", O DBType("text"))
  def time = column[Timestamp]("time")
  def doing = column[String]("doing", O DBType("text"))
  def channel = column[String]("channel", O DBType("text"))
  def saying = column[Option[String]]("saying", O DBType("text"))
  def saying_time = column[Option[Timestamp]]("saying_time")
  def nick_index = index("nick_index", nick)
  def * = id ~ nick ~ time ~ doing ~ channel ~ saying ~ saying_time
  def all = nick ~ time ~ doing ~ channel ~ saying ~ saying_time
}
