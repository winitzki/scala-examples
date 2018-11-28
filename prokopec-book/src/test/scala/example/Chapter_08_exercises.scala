package example

import java.util.{Timer, TimerTask}

import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.actor._
import akka.event.Logging
import io.chymyst.jc._

class Chapter_08_exercises extends FlatSpec with Matchers {
  behavior of "actors"

  it should "run hello actor" in {
    lazy val ourSystem = ActorSystem("OurExampleSystem")

    object HelloActor {
      def props(hello: String) = Props(new HelloActor(hello))
    }
    class HelloActor(val hello: String) extends Actor {
      def receive = {
        case `hello` ⇒
          println(s"Received a '$hello'... $hello!")
        case msg ⇒
          println(s"Unexpected message '$msg'")
          context.stop(self)
      }
    }
    val hiActor: ActorRef = ourSystem.actorOf(HelloActor.props("hi"), name = "greeter")
    hiActor ! "hi"
    Thread.sleep(1000)
    hiActor ! "hola"
    Thread.sleep(1000)
    hiActor ! "hi"
    Thread.sleep(1000)
    Await.result(ourSystem.terminate(), Duration.Inf)
  }

  it should "run hello chymyst" in {
    // Accept given greeting but stop running when the message does not contain that greeting.
    def acceptHello(hello: String): M[String] = {
      val helloM = m[String]
      val enabled = m[Unit]
      val pool = FixedPool()
      site(pool) {
        go { case helloM(str) + enabled(_) ⇒
          str match {
            case `hello` ⇒
              println(s"Received a '$hello'... $hello!")
              enabled()
            case msg ⇒
              println(s"Unexpected message '$msg'")
          }
        }
      }
      enabled()
      helloM
    }

    val h = acceptHello("hi")
    h("hi")
    Thread.sleep(1000)
    h("hola")
    Thread.sleep(1000)
    h("hi")
    Thread.sleep(1000)
  }

  it should "exercise 1" in {
    lazy val ourSystem = ActorSystem("OurExampleSystem")
    final case class Register(t: Long)
    final case class Begin(t: Long, ta: ActorRef)
    final case class Timeout(t: Long)
    class TimerActor extends Actor {
      val timer = new Timer // java.util.Timer

      override def receive: Receive = {
        case Register(t) ⇒
          val theSender = sender() //  Need to capture the sender here.
          timer.schedule(new TimerTask {
            override def run(): Unit = theSender ! Timeout(t) // Should not call `sender()` here.
          }, t)
      }
    }
    val timerActor: ActorRef = ourSystem.actorOf(Props(new TimerActor))

    // Create two test actors that will send messages to timerActor.

    class TestActor(name: String) extends Actor {
      override def receive: Receive = {
        case Begin(t, ar) ⇒ ar ! Register(t)
        case Timeout(t) ⇒ println(s"Actor $name received a timeout signal with duration $t")
      }
    }
    def makeTestActor(name: String): ActorRef = ourSystem.actorOf(Props(new TestActor(name)))

    val testActor1 = makeTestActor("1")
    val testActor2 = makeTestActor("2")

    testActor1 ! Begin(500, timerActor)
    Thread.sleep(200)
    testActor2 ! Begin(100, timerActor)
    Thread.sleep(1000)
    /* Output:
Actor 2 received a timeout signal with duration 100
Actor 1 received a timeout signal with duration 500
    */
    Await.result(ourSystem.terminate(), Duration.Inf)
  }

  it should "exercise 1 chymyst" in {
    val timerM = m[Timer]
    val registerM = m[(Long, M[Long])]

    val testSender1 = new M[Long]("1")
    val testSender2 = new M[Long]("2")

    def makeTestSender(testSender: M[Long]): Reaction = go { case testSender(t) ⇒ println(s"Sender ${testSender.name} got message with duration $t") }

    val pool = FixedPool()
    site(pool)(
      go { case registerM((t, theSender)) + timerM(timer) ⇒
        timer.schedule(new TimerTask {
          override def run(): Unit = theSender(t)
        }, t)
        timerM(timer)
      }
      , makeTestSender(testSender1)
      , makeTestSender(testSender2)
    )
    timerM(new Timer)
    registerM((500, testSender1))
    Thread.sleep(200)
    registerM((100, testSender2))
    Thread.sleep(1000)
    /* Output:
Sender 2 got message with duration 100
Sender 1 got message with duration 500
    */
    pool.shutdownNow() // Optional.
  }

  it should "exercise 2" in {
    lazy val ourSystem = ActorSystem("OurExampleSystem")
    final case class SendAmt(amount: Long, target: ActorRef)
    final case class ReceiveAmt(amount: Long)
    class AccountActor(name: String) extends Actor {
      var currentAmount: Long = 0

      override def receive: Receive = {
        case SendAmt(amount, targetActorRef) ⇒
          currentAmount -= amount
          targetActorRef ! ReceiveAmt(amount)
          println(s"Account $name changes balance to $currentAmount")
        case ReceiveAmt(amount) ⇒
          currentAmount += amount
          println(s"Account $name changes balance to $currentAmount")
      }
    }
    val acc1 = ourSystem.actorOf(Props(new AccountActor("Person_1")))
    val acc2 = ourSystem.actorOf(Props(new AccountActor("Person_2")))

    acc1 ! ReceiveAmt(1000)
    acc2 ! ReceiveAmt(2000)
    acc1 ! SendAmt(500, acc2)
    acc2 ! SendAmt(1000, acc1)
    /* Output:
Account Person_1 changes balance to 1000
Account Person_2 changes balance to 2000
Account Person_1 changes balance to 500
Account Person_2 changes balance to 1000
Account Person_1 changes balance to 1500
Account Person_2 changes balance to 1500   
     */
    Await.result(ourSystem.terminate(), Duration.Inf)
  }


}
