package com.tedpearson.troggie
import java.util.Properties

class TestPlugin(conf: PluginConf) extends Plugin(conf) {
	protected def processMessage(m: IrcMessage) {
		m match {
			case m: Message => {
				Console.println("Message" + m)
				sender.tell("hello")
			}
			case _ =>
		}
	}
} 