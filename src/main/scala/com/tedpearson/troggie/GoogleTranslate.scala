package com.tedpearson.troggie

class GoogleTranslate(conf: PluginConf) extends Plugin(conf) {
	protected def processMessage(m: IrcMessage): Unit = {
		m match {
			case n: Notice => println("HELLOOOOOO")
		}
	}
}