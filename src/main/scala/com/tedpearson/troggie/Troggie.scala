package com.tedpearson.troggie

import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileWriter
import java.sql.DriverManager
import java.util.Properties

import org.jibble.pircbot.PircBot

import akka.actor._
import akka.routing.BroadcastRouter

class Troggie(network: String) extends PircBot with Actor {
  val version = "3.0b1"
  import Troggie._ => t
  val logfile = new File(properties.getProperty("logfile","troggie.log"))
  logfile.createNewFile
  val log = new BufferedWriter(new FileWriter(logfile, true))
  setVerbose(true)
  val nick = properties.getProperty("nick")
  setName(nick)
  setLogin(nick)
  setEncoding("UTF-8")
  val port = Integer.parseInt(properties.getProperty("port", "6667"))
  val router = loadPlugins
  log(router.toString)
  
  // don't connect until actor is started
  // otherwise we try to start sending messages before akka is ready
  override def preStart() {
    connect(network, port, properties.getProperty("server_password"))
    joinChannels
  }
  
  // disconnect when akka is shut down so the program will die appropriately
  override def postStop() {
    dispose
  }

  private def joinChannels {
    for(c <- properties.getProperty(network + "_channels").split(",")) {
      joinChannel(c)
    }
  }
  
  private def loadPlugins: ActorRef = {
    val conf = new PluginConf(properties, conn)
    val active = for(plugin <- properties.getProperty("plugins").split(",") if pluginDefined(plugin))
      yield context.actorOf(Props(
          Class.forName(plugin).getConstructor(classOf[PluginConf]).newInstance(conf).asInstanceOf[Plugin]), name=plugin)
    val router = context.actorOf(Props[Plugin].withRouter(BroadcastRouter(routees = active)))
    log(active.size.toString())
    log(router.toString)
    router
  }
  
  private def pluginDefined(plugin: String) = {
    try {
      Class.forName(plugin)
      true
    } catch {
      case e: Exception => e.printStackTrace
          false
    }
  }
  
  def receive = {
    case _ => log("unknown msg")
  }

  override def log(line: String) {
    if(!line.contains("PING :") & !line.contains("PONG :")) {
      val toLog = scala.compat.Platform.currentTime + " " + line
          log.write(toLog)
          log.write(self.path.name)
          log.newLine()
          log.flush
          Console.println(toLog)
    }
  }
  
  override def onVersion(sourceNick: String, sourceLogin: String, sourceHostname: String, target: String) {
    sendRawLine("NOTICE %s :\u0001VERSION Troggie %s - https://github.com/tedpearson/Troggie (A %s)\u0001"
        format(sourceNick, version, getVersion()));
  }
  
  override def onDisconnect() {
    while(!isConnected()) {
      try {
        reconnect
        joinChannels
      } catch {
        case _ => Thread sleep 5000
      }
    }
  }
  
  override def onMessage(channel: String, sender: String, login: String, host: String, msg: String) {
    router ! Message(channel, sender, login, host, msg)
  }
  
  override def onPrivateMessage(sender: String, login: String, host: String, msg: String) {
    router ! PrivateMessage(sender, login, host, msg)
  }
  
  override def onAction(channel: String, sender: String, login: String, host: String, msg: String) {
    router ! Action(channel, sender, login, host, msg)
  }
  
  override def onDeop(channel: String, sender: String, login: String, host: String, rcpt: String) {
    router ! Deop(channel, sender, login, host, rcpt)
  }
  
  override def onDeVoice(channel: String, sender: String, login: String, host: String, rcpt: String) {
    router ! DeVoice(channel, sender, login, host, rcpt)
  }
  
  override def onJoin(channel: String, sender: String, login: String, hostname: String) {
    sender match {
      // backticks required with lowercase variable here
      // otherwise it will assume we're declaring a new variable to bind on match
      case `nick` => router ! SelfJoin(channel)
      case _ => router ! Join(channel, sender, login, hostname)
    }
  }
  
  override def onKick(channel: String, sender: String, login: String, host: String, rcpt: String, msg: String) {
    rcpt match {
      case `nick` => {
        Thread sleep 1000
        joinChannel(channel)
      }
    }
    router ! Kick(channel, sender, login, host, rcpt, msg)
  }
  
  override def onNickChange(oldNick: String, login: String, host: String, newNick: String) {
    oldNick match {
      case `nick` => router ! SelfNickChange(oldNick, login, host, newNick)
      case _ => router ! NickChange(oldNick, login, host, newNick)
    }
  }
  
  override def onNotice(sender: String, login: String, host: String, target: String, msg: String) {
    router ! Notice(sender, login, host, target, msg)
  }
  
  override def onOp(channel: String, sender: String, login: String, host: String, rcpt: String) {
    router ! Op(channel, sender, login, host, rcpt)
  }
  
  override def onPart(channel: String, sender: String, login: String, host: String) {
    router ! Part(channel, sender, login, host)
  }
  
  override def onQuit(sender: String, login: String, host: String, msg: String) {
    router ! Quit(sender, login, host, msg)
  }
  
  override def onTopic(channel: String, topic: String, setBy: String, date: Long, change: Boolean) {
    router ! Topic(channel, topic, setBy, date, change)
  }
  
  override def onVoice(channel: String, sender: String, login: String, host: String, rcpt: String) {
    router ! Voice(channel, sender, login, host, rcpt)
  }
}

object Troggie extends App {
  var confFile = "troggie.conf"
  val properties = new Properties
  if(args.length != 0) {
    confFile = args(0)
  }
  val propFile = new File(confFile)
  if(propFile.createNewFile()) {
    properties.setProperty("nick", "troggi");
    properties.setProperty("networks", "irc.freenode.net");
    properties.setProperty("irc.freenode.net_channels", "#troggie");
    properties.setProperty("plugins", "com.tedpearson.troggie.Factoid"
        + ",com.tedpearson.troggie.Seen"
        + ",com.tedpearson.troggie.GoogleSearch"
        + ",com.tedpearson.troggie.UrlTitle"
        + ",com.tedpearson.troggie.GoogleTranslate");
    properties.setProperty("database","troggie-db");
    properties.store(new FileOutputStream(propFile), "")
  }
  properties.load(new FileInputStream(propFile))
  val networks = properties.getProperty("networks").split(",")
  
  val conn = dbConnect(properties.getProperty("database"))
  
  val system = ActorSystem("Troggie")
  
  for(n <- networks) {
    system.actorOf(Props(new Troggie(n)), name=n)
  }
  
  Thread.currentThread.getName match {
    case "main" =>
    case _ => {
      pressKeyToStop
    }
  }

  def pressKeyToStop {
    while(System.in.available() <= 0) {
      try { Thread.sleep(1000) } catch { case _: InterruptedException => () }
    }
    system.shutdown()
  }
  
  private def dbConnect(database: String) = {
     try {
      Class.forName("org.sqlite.JDBC")
    } catch {
      case e: Exception => e.printStackTrace()
          System.exit(1)
    }
    DriverManager.getConnection("jdbc:sqlite:%s" format database)
  }
}