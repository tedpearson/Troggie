package com.tedpearson.troggie

class GoogleTranslate(conf: PluginConf) extends Plugin(conf) {
  override protected def getStatusString = ""
  protected def processMessage(m: IrcMessage): Unit = {
    m match {
      case n: Notice => 
      case _ =>
    }
  }
}