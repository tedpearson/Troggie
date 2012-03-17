package com.tedpearson.troggie
import java.net.URL
import javax.swing.text.html.parser.ParserDelegator
import java.io.InputStreamReader
import javax.swing.text.html.HTMLEditorKit
import javax.swing.text.html.HTML
import javax.swing.text.MutableAttributeSet
import java.io.IOException
import akka.dispatch.Future

class UrlTitle(conf: PluginConf) extends Plugin(conf) {
  implicit val system = context.system
  protected def processMessage(message: IrcMessage): Unit = message match {
    case m: Action => getUrlTitle(m.action, m.target, m.sender)
    case m: PublicMessage => getUrlTitle(m.msg, m.channel, m.sender)
    case _ => 
  }
  
  var count = 0
  def getStatusString = " Urls retrieved: %d." format count
  
  val Url = """(https?://[^ ]+)""".r
  private def getUrlTitle(msg: String, target: String, sender: String): Unit = msg match {
    case Url(url) => Future {
      val conn = new URL(url).openConnection()
      conn.setRequestProperty("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_2)" +
          " AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11")
      conn.connect
      val input = conn.getInputStream
      if(conn.getHeaderField("Content-Type").contains("text/html")) {
        try {
          new ParserDelegator().parse(new InputStreamReader(input), new HTMLEditorKit.ParserCallback {
            var foundTitle = false
            override def handleStartTag(t: HTML.Tag, a: MutableAttributeSet, pos: Int): Unit = t match {
              case HTML.Tag.TITLE => foundTitle = true
              case _ =>
            }
            override def handleEndTag(t: HTML.Tag, pos: Int): Unit = t match {
              case HTML.Tag.TITLE => {
                foundTitle = false
                input.close
              }
              case _ =>
            }
            override def handleText(data: Array[Char], pos: Int): Unit = if(foundTitle) {
              count += 1
              troggie ! SendMessage(target, "[ %s ]" format new String(data), true)
            }
          }, true)
        } catch {
          case e: IOException => {
            if(!e.getMessage().contains("stream is closed")) {
              troggie ! SendMessage(target, "Error accessing '%s'" format url, true)
            }
          }
        }
      } else {
        input.close
        return
      }
    }
    case _ => return
  }
}