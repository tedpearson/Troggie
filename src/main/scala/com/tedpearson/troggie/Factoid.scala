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
  implicit val timeout = Timeout(5000L)
  import conf.p
  val doPrivate = p.getProperty("enable_privateupdate") == "true"
  val maxKeyLen = p.getProperty("infobot_keylen", "60").toInt
  val maxValLen = p.getProperty("infobot_vallen", "500").toInt
  val volunteerLen = p.getProperty("infobot_volunteer_length", "8").toInt
  val db = context.actorOf(Props(new FactoidDb))
  var (modifications, questions, nick) = (0,0,"")
  db ! Setup
  
  override protected def getStatusString = {
    val count = Await.result(db ? Count, timeout.duration)
    "Factoids modified/queried: %n/%n. %n Factoids currently exist." format(modifications, questions, count)
  }
  
  val Channel = """#.+""".r
  protected def processMessage(message: IrcMessage): Unit = message match {
    case m: PublicMessage => matchMessage(m.channel, m.sender, m.msg, false)
    case m: PrivateMessage => matchMessage(m.sender, m.sender, m.msg, true)
    case m: Action => m.target match {
      case Channel() =>  matchMessage(m.target, m.sender, m.action, false)
      case _ => matchMessage(m.sender, m.sender, m.action, true)
    }
    case m: SelfNickChange => {
      nick = m.newNick
      SetFact = """(?i)(no,?\s+(?:%s,?)?\s+)?(.+?)\s+(is|are)\s+(.+)""".format(nick).r
    }
    case _ =>
  }
  
  val Fix = """(?i)(?:what|where|who)(?:\s+(?:is|are)|\'s|\'re)\s+(.+)$""".r
  val Iam = "(?i)^i am".r
  val FactNumber = """(?i)fact(?:oid)? #?(\d+)\?*""".r
  // initially will respond to any addressing
  var SetFact = """(?i)(no,?\s+(?:\w+,?)?\s+)?(.+?)\s+(is|are)\s+(.+)""".r
  val ForgetFact = """(?i)forget (.+)""".r
  val LiteralFact = """(?i)literal (.+)""".r
  val RandomFact = """(?i)random fact\?*""".r
  
  def matchMessage(target: String, sender: String, message: String, isPrivate: Boolean) {
    val Addressed = """^%s\s*(?:[:,]|\s+)(.+)""".format(nick).r
    var (msg, addressed) = message.trim() match {
      case Addressed(str) => (str, true)
      case msg => (msg, false)
    }
    msg = msg match {
      case Fix(str) => str.replace(" is ", """ \is """).replace(" are ",""" \are """)
      case _ => msg
    }
    msg = Iam.replaceFirstIn(msg, "%s is ".format(sender))
    msg match {
      case FactNumber(num) =>
      case SetFact(no, key, isAre, value) =>
      case ForgetFact(key) =>
      case LiteralFact(key) =>
      case RandomFact() =>
    }
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