package com.tedpearson.troggie
import java.util.Properties

class Seen(conf: PluginConf) extends Plugin(conf) {
	private val SEEN_TABLE = "TROGGIE_SEEN"
  setupDb
  
  
	
  protected def processMessage(message: IrcMessage) {
    message match {
    	case j: Join => updateDb(j.sender, "joining the channel", j.channel)
    	case _ =>
    }
  }
  
  private def updateDb(sender: String, doing: String, channel: String) {
  	println("HELLODERE" + sender)
  }
  
  private def setupDb {
    val rs = conf.conn.getMetaData().getTables(null, null, SEEN_TABLE, null)
    val tableExists = rs.next
    rs.close
    if(tableExists) {
    	
    }
  }
}