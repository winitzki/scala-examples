package example

import org.scalatest.{FlatSpec, Matchers}

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, LinkOption, Paths}
import scala.sys.process._

class JavaIoFileBehavior extends FlatSpec with Matchers {

  it should "determine that symbolic link does not exist" in {
    "rm -f /tmp/nonexistent_file /tmp/existent_file /tmp/nonexistent_link /tmp/existent_link".!
    "ln -s /tmp/nonexistent_file /tmp/nonexistent_link".!
    Files.write(Paths.get("/tmp/existent_file"), "".getBytes)
    "ln -s /tmp/existent_file /tmp/existent_link".!
    new java.io.File("/tmp/nonexistent_link").exists shouldEqual false
    new java.io.File("/tmp/existent_link").exists shouldEqual true
    new java.io.File("/tmp/nonexistent_file").lastModified shouldEqual 0

    Files.exists(Paths.get("/tmp/existent_link")) shouldEqual true
    Files.exists(Paths.get("/tmp/nonexistent_link")) shouldEqual false
    Files.exists(Paths.get("/tmp/nonexistent_link"), LinkOption.NOFOLLOW_LINKS) shouldEqual true
    Files.getLastModifiedTime(Paths.get("/tmp/nonexistent_link"), LinkOption.NOFOLLOW_LINKS) should be > FileTime.fromMillis(0)
    the[java.nio.file.NoSuchFileException] thrownBy Files.getLastModifiedTime(Paths.get("/tmp/nonexistent_link")) should have message "/tmp/nonexistent_link"
  }
}
