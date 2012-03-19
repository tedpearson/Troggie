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
  val maxKeyLen = p.getProperty("infobot_keylen", "30").toInt
  val maxValLen = p.getProperty("infobot_vallen", "500").toInt
  val volunteerLen = p.getProperty("infobot_volunteer_length", "8").toInt
  import FactoidDb._
  var (modifications, queries, nick) = (0,0,"")
  setup
  
  override protected def getStatusString = {
    val count = FactoidDb.count
    "Factoids modified/queried: %d/%d. %d Factoids currently exist." format(modifications, queries, count)
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
      SetFact = SetFactRE.format(nick).r
    }
    case _ =>
  }
  
  val Fix = """(?i)(?:what|where|who)(?:\s+(?:is|are)|\'s|\'re)\s+(.+)$""".r
  val Iam = "(?i)^i am".r
  val FactNumber = """(?i)fact(?:oid)? #?(\d+)\?*""".r
  // initially will respond to "troggie"
  val SetFactRE = """(?i)(no,?(?:\s+%s,?)?\s+)?(.+?)\s+(is|are)(\s+also)?\s+(.+)\s*"""
  var SetFact = SetFactRE.format("troggie").r
  val ForgetFact = """(?i)forget\s+(.+)\s*""".r
  val LiteralFact = """(?i)literal\s+(.+)\s*\?*\s*""".r
  val RandomFact = """(?i)random fact\?*""".r
  
  def matchMessage(target: String, sender: String, message: String, isPrivate: Boolean) {
    val Addressed = """^%s\s*[:,]?\s*(.+)""".format(nick).r
    var (msg, addressed) = message.trim() match {
      case Addressed(str) => (str, true)
      case msg => (msg, false)
    }
    msg = msg match {
      case Fix(str) => str.replace(" is ", """ \is """).replace(" are ",""" \are """)
      case _ => msg
    }
    msg = Iam.replaceFirstIn(msg, "%s is ".format(sender))
    implicit val p: Boolean = isPrivate
    msg match {
      case FactNumber(num) => {
        queries += 1
        findById(num.toInt) match {
          case Some((key, value, isAre)) => 
            send(target, "Factoid #%s: %s %s %s".format(num, key, isAre, value))
          case None => send(target, "Factoid #%s does not exist." format num)
        }
      }
      case SetFact(no, key, isAre, also, value) => pck {
        val force = no != null
        // i think this is so you can escape these when you're setting the value in chat
        val output = value.replaceAll("""(?i)(?:\\(who )|\\(is )|\\(are )|\\(what ))""", "$1$2$3$4")
        // if value is too long, truncate (shouldn't happen unless on a irc
        // network that allows long strings)
        if(key.length > maxKeyLen && addressed) {
          val str = "Max length for input is %d characters, %s. " +
          		"Please try a shorter value before the 'is'."
          send(target, str.format(maxKeyLen, sender))
      		return
        }
        if(output.length > maxValLen && addressed) {
          send(target, "Max length for output is %d characters, %s. Response truncated."
              .format(maxKeyLen, sender))
          return
        }
        find(key) match {
          case Some((origKey, origValue, origIsAre)) => {
            val isAlso = also != null
            if(isAlso) {
              modifications += 1
              concat(key, " " + output)
              send(target, "okay, %s." format sender)
              return
            }
            if(origValue == output) {
              send(target, "I already had it that way, %s." format sender)
              return
            }
            force match {
              case true => {
                modifications += 1
                update(key, value, isAre)
                send(target, "okay, %s." format sender)
              }
              case false => if(addressed) {
                send(target, "... but %s %s %s ...".format(key, origIsAre, origValue))
              }
            }
          }
          case None => {
            modifications += 1
            update(key, value, isAre)
          }
        }
      }
      case ForgetFact(k) => pck {
        val key = if(k.equalsIgnoreCase("me")) sender else k
        find(key) match {
          case Some(_) => {
            delete(key)
            modifications += 1
            send(target, "I forgot '%s', %s".format(key, sender))
          }
          case None => send(target, "I don't have anything matching '%s', %s".format(key, sender))
        }
      }
      case LiteralFact(k) => {
        queries += 1
        find(k) match {
          case Some((key, value, isAre)) => send(target, "%s =%s= %s".format(key, isAre, value))
          case None => send(target, "I don't have anything matching '%s', %s".format(k, sender))
        }
      }
      case RandomFact() => {
        queries += 1
        val max = maxId.get
        import util.control.Breaks._
        breakable {
          import util.Random.nextInt
          import akka.dispatch.Future
          implicit val ec = context.system
          // don't let this block the actor
          Future {
            while(true) {
              val random = nextInt(max + 1)
              findById(random) collect {
                case (key, value, isAre) => {
                  send(target, "Factoid #%d: %s %s %s".format(random, key, isAre, value))
                  break
                }
              }
            }
          }
        }
      }
      case _ => {
        // check if it's stored in the db
      }
    }
  }
  
  def pck[A](a: => A)(implicit isPrivate: Boolean) = if(!isPrivate || doPrivate) {
    a
  }
  
  object FactoidDb {
    def setup = createIfNotExists(IsTable)
    def update(key: String, value: String, isAre: String) {
      val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i.edit
      if(row.firstOption.isEmpty) IsTable.all insert (key, value, isAre)
      else row.update((value, isAre))
    }
    def find(key: String) = {
      val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i.all
      row.firstOption
    }
    def delete(key: String) {
      val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i
      row.delete
    }
    def concat(key: String, also: String) {
      val row = for(i <- IsTable if lowerc(i.key) === lowers(key)) yield i.value
      row.firstOption match {
        case Some(s) => {
          row.update(s ++ also)
        }
        case None =>
      }
    }
    def count = Query(IsTable.count).first
    def findById(id: Int) = {
      val row = for(i <- IsTable if i.id === id) yield i.all
      row.firstOption
    }
    def maxId = {
      Query(IsTable.id.max).first
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