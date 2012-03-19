package com.tedpearson.troggie
import org.jibble.pircbot.User

sealed trait TroggieMessage

sealed trait IrcMessage extends TroggieMessage
sealed abstract class NickMessage extends IrcMessage {
  val sender: String
}

sealed abstract class Message extends NickMessage {
  val sender: String
  val login: String
  val host: String
  val msg: String
}
case class PublicMessage(channel: String, sender: String, login: String, host: String, msg: String) 
  extends Message
case class PrivateMessage(sender: String, login: String, host: String, msg: String)
  extends Message
case class Action(sender: String, login: String, host: String, target: String, action: String) extends NickMessage
case class Deop(channel: String, sender: String, login: String, host: String, rcpt: String) extends NickMessage
case class DeVoice(channel: String, sender: String, login: String, host: String, rcpt: String) extends NickMessage
case class Join(channel: String, sender: String, login: String, host: String) extends NickMessage
case class Kick(channel: String, sender: String, login: String, host: String, rcpt: String, msg: String) extends NickMessage
case class NickChange(sender: String, login: String, host: String, newNick: String) extends NickMessage
case class Notice(sender: String, login: String, host: String, target: String, msg: String) extends NickMessage
case class Op(channel: String, sender: String, login: String, host: String, rcpt: String) extends NickMessage
case class Part(channel: String, sender: String, login: String, host: String) extends NickMessage
case class Quit(sender: String, login: String, host: String, msg: String) extends NickMessage
case class Topic(channel: String, topic: String, sender: String, date: Long, change: Boolean) extends NickMessage
case class Voice(channel: String, sender: String, login: String, host: String, rcpt: String) extends NickMessage
case class SelfJoin(channel: String) extends IrcMessage
case class UserList(users: Array[User]) extends IrcMessage
case class SelfNickChange(newNick: String) extends IrcMessage
case class GetStatus() extends IrcMessage


sealed trait PluginMessage extends TroggieMessage

case class SendMessage(target: String, msg: String) extends PluginMessage
case class SendNotice(target: String, notice: String) extends PluginMessage
case class SendAction(target: String, msg: String) extends PluginMessage
case class GetUsers(channel: String) extends PluginMessage
case class VoiceUser(channel: String, user: String) extends PluginMessage
case class Log(msg: String) extends PluginMessage
case class Status(status: String) extends PluginMessage