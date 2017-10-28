package example

object HelloWorld {

  def computeMessage(): String = "hello: "

  def computeNumber(): Int = 123

  def main(args: Array[String]): Unit = {
    val message = computeMessage()

    val number = computeNumber()

    println(message + number)
  }
}
