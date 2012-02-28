package com.tedpearson.troggie
import org.jibble.pircbot.User

sealed trait TroggieMessage

sealed trait IrcMessage extends TroggieMessage

case class Message(channel: String, sender: String, login: String, host: String, msg: String) extends IrcMessage
case class PrivateMessage(sender: String, login: String, host: String, msg: String) extends IrcMessage
case class Action(channel: String, sender: String, login: String, host: String, msg: String) extends IrcMessage
case class Deop(channel: String, sender: String, login: String, host: String, rcpt: String) extends IrcMessage
case class DeVoice(channel: String, sender: String, login: String, host: String, rcpt: String) extends IrcMessage
case class Join(channel: String, sender: String, login: String, host: String) extends IrcMessage
case class Kick(channel: String, sender: String, login: String, host: String, rcpt: String, msg: String) extends IrcMessage
case class NickChange(oldNick: String, login: String, host: String, newNick: String)
case class Notice(sender: String, login: String, host: String, target: String, msg: String) extends IrcMessage
case class Op(channel: String, sender: String, login: String, host: String, rcpt: String) extends IrcMessage
case class Part(channel: String, sender: String, login: String, host: String) extends IrcMessage
case class Quit(sender: String, login: String, host: String, msg: String) extends IrcMessage
case class Topic(channel: String, topic: String, setBy: String, date: Long, change: Boolean) extends IrcMessage
case class Voice(channel: String, sender: String, login: String, host: String, rcpt: String) extends IrcMessage
case class SelfJoin(channel: String) extends IrcMessage
case class UserList(users: Array[User]) extends IrcMessage
case class SelfNickChange(oldNick: String, login: String, host: String, newNick: String) extends IrcMessage


sealed trait PluginMessage extends TroggieMessage

case class SendMessage(target: String, msg: String, count: Boolean) extends PluginMessage
case class SendNotice(target: String, notice: String) extends PluginMessage
case class SendAction(target: String, msg: String) extends PluginMessage
case class GetUsers(channel: String) extends PluginMessage
case class VoiceUser(channel: String, user: String) extends PluginMessage
case class Log(msg: String)