package example

import WuZip.WuZipSyntax
import cats.Functor
import cats.syntax.functor._
import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.ch._

class Chapter_02_exercises extends FlatSpec with Matchers {

  behavior of "exercises"

  it should "implement exercise 1" in {
    def parallel[A, B](a: ⇒ A, b: ⇒ B): (A, B) = {
      var resultA: A = null.asInstanceOf[A]
      var resultB: B = null.asInstanceOf[B]
      
      val threadA = new Thread {
        resultA = a
      }
      threadA.start()
      
      val threadB = new Thread {
        resultB = b
      }
      threadB.start()
      
      threadA.join()
      threadB.join()
      
      (resultA, resultB)
    }

    (0 to 100).foreach { i ⇒
      parallel(s"first thread $i", s"second thread $i") shouldEqual ((s"first thread $i", s"second thread $i"))
    }
  }
  
  it should "implement exercise 2" in {
    
  }
}
