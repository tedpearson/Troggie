package com.tedpearson.troggie
import java.util.Properties
import org.junit.runner.RunWith
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.session.Database
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.scalatest.TestFailedException
import org.specs.mock.Mockito
import org.specs.specification.DefaultExampleExpectationsListener
import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.TestActorRef
import org.scalatest.junit.JUnitRunner
import java.sql.Timestamp
import org.scalatest.BeforeAndAfter

@RunWith(classOf[JUnitRunner])
class SeenTest extends FlatSpec with BeforeAndAfter with ShouldMatchers with Mockito with DefaultExampleExpectationsListener {
  implicit val system = ActorSystem("test")
  implicit val session = Database.forURL("jdbc:sqlite::memory:", driver = "org.sqlite.JDBC").createSession()
  val conf = new PluginConf(mock[Properties], session)
  val impl = TestActorRef(Props(new Seen(conf))).underlyingActor.asInstanceOf[Seen]
  val e = "";
  
  after {
    (for(s <- SeenTable) yield s).delete
  }
  
  "processMessage" should "correctly process Join" in {
    impl.processMessage(Join("#ignmac", "Burni", e, e))
    val line = getLine("Burni")
    line match {
      case (id, "Burni", a, "joining the channel", "#ignmac", c, d) => 
        id should equal (1)
        a.getTime() should be > (0L)
        c.isDefined should be (false)
        d.isDefined should be (false)
      case _ => error(line.toString)
    }
  }
  
  it should "correctly process Message" in {
    impl.processMessage(Message("#ignmac", "Burni", e, e, "This is a test"))
    var line = getLine("Burni")
    line match {
      case (id, "Burni", a, "saying: 'This is a test'", "#ignmac", Some("This is a test"), c) =>
        id should equal (1)
        a.getTime() should be > (0L)
        c.isDefined should be (true)
      case _ => error(line.toString)
    }
  }
  
  it should "correclty process Action" in {
    impl.processMessage(Action("Burni", e, e, "#ignmac", "This is a test"))
    var line = getLine("Burni")
    line match {
      case (id, "Burni", a, "saying: '*Burni This is a test'", "#ignmac", Some("*Burni This is a test"), c) =>
        id should equal(1)
        a.getTime() should be > (0L)
        c.isDefined should be (true)
      case _ => error(line.toString)
    }
  }
  
  def getLine(n: String) = {
    (for(s <- SeenTable if s.nick === n) yield s).first
  }
  
  def error(is: String) {
    throw new TestFailedException("%s did not match correctly" format is, 2)
  }
}