package com.tedpearson.troggie
import DbAccess._
import org.scalaquery.ql.basic.BasicTable
import java.sql.Timestamp
import org.scalaquery.meta.MTable
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql._
import org.scalaquery.session.Session
import scala.compat.Platform.currentTime

class Seen(conf: PluginConf) extends Plugin(conf) {
  implicit val session: Session = conf.session
  setupDb
	
  protected def processMessage(message: IrcMessage) {
    message match {
    	case j: Join => updateDb(j.sender, "joining the channel", j.channel)
    	case m: Message => updateDb(m.sender, "saying", m.channel, Some(m.msg))
    	case _ =>
    }
  }
  
  private def setupDb {
  	if(MTable.getTables.firstOption.isEmpty) SeenTable.ddl.create
  }
  
  private object SeenTable extends BasicTable[(Int, String, Timestamp, String, String, Option[String], Option[Timestamp])]("TROGGIE_SEEN") {
    def id = column[Int]("id", O PrimaryKey)
    def nick = column[String]("nick", O DBType("text"))
    def time = column[Timestamp]("time")
    def doing = column[String]("doing", O DBType("text"))
    def channel = column[String]("channel", O DBType("text"))
    def saying = column[Option[String]]("saying", O DBType("text"))
    def saying_time = column[Option[Timestamp]]("saying_time")
    def nick_index = index("nick_index", nick)
    def * = id ~ nick ~ time ~ doing ~ channel ~ saying ~ saying_time
  }
  
  private def updateDb(doing: String, channel: String, nick: String, saying: Option[String] = None) {
    println("UPDATE" + nick)
  	val row = for(s <- SeenTable if s.nick is nick) yield s
    val time = new Timestamp(currentTime)
    val sTime = if(saying.isDefined) Some(time) else None
  	if(row.firstOption.isDefined) 
  		row.update((0, nick, time, doing, channel, saying, sTime))
  	else
  		SeenTable insert (0, nick, time, doing, channel, saying, sTime)
  }
}

