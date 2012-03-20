package com.tedpearson.troggie
import java.util.Properties
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.specs.mock.Mockito
import org.specs.specification._
import akka.actor.ActorSystem
import akka.actor.Props
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterAll
import org.scalaquery.session.Session
import akka.testkit.TestKit
import akka.testkit.ImplicitSender
import akka.actor.ActorRef
/*
@RunWith(classOf[JUnitRunner])
class SeenTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with FlatSpec
with ShouldMatchers with BeforeAndAfterAll with DefaultExampleExpectationsListener with Mockito {
  def this() = this(ActorSystem("Test"))
  val conf = new PluginConf(mock[Properties], mock[Session])
  val db = mock[SeenDb]
  val impl = system.actorOf(Props(new NewSeen(conf, mock)))
  val e = "";

  "processMessage" should "correctly process Join" in {
    impl ! Join("#ignmac", "Burni", e, e)
    there was one(db).update("Burni", "joining the channel", "#ignmac", None)
  }
}

class NewSeen(conf: PluginConf, data: SeenDb) extends Seen(conf) {
  override def db = data
}*/