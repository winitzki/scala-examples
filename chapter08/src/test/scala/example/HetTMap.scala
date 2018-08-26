package example

import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.Manifest

// Immutable map from `K[A]` to `V[A]`, where the type parameter `A` can be different for different map entries.
// Uses standard Map() as data structure, with type parameters `Map[(K[_], Manifest[_]), HetTMEntry[K, V]]`.
final case class HetTMap[K[_], V[_]](
  data: Map[(K[_], Manifest[_]), V[_]] = Map[(K[_], Manifest[_]), V[_]]()
) {
  // Fetch entry.
  def get[A](key: K[A])(implicit manifest: Manifest[A]): Option[V[A]] = {
    val k: (K[_], Manifest[_]) = (key, manifest)
    data.get(k).map(_.asInstanceOf[V[A]]) // Since we have manifest: Manifest[A], we are guaranteed that the erased type is A.
  }

  // Add/replace entry in map.
  def updated[A](key: K[A], value: V[A])(implicit manifest: Manifest[A]): HetTMap[K, V] = {
    val k: (K[_], Manifest[_]) = (key, manifest)
    HetTMap[K, V](data.updated(k,  value))
  }

  // Remove key from map.
  def -[A](key: K[A])(implicit manifest: Manifest[A]): HetTMap[K, V] = {
    val k: (K[_], Manifest[_]) = (key, manifest)
    HetTMap[K, V](data - k)
  }

  // Some standard convenience methods.
  def size: Int = data.size

  def isEmpty: Boolean = data.isEmpty
}

class HetTMapSpec extends FlatSpec with Matchers {

  behavior of "heterogeneous type-parameterized map"

  type Foo[A] = Option[List[A]]
  type Bar[A] = A

  it should "create an empty map" in {
    val m = HetTMap[Foo, Bar]()
    m.size shouldEqual 0
    m.isEmpty shouldEqual true
  }

  it should "add entry to map and then fetch it" in {
    val m = HetTMap[Foo, Bar]()

    m.updated(Some(List(1)), 1)
      .get(Some(List(1))) shouldEqual Some(1)
  }

  it should "add entry with different type parameters, fetch, and delete them" in {
    val m = HetTMap[Foo, Bar]()
      .updated(Some(List(1)), 1)
      .updated(Some(List("a")), "b")

    m.size shouldEqual 2
    m.isEmpty shouldEqual false

    m.get(Some(List(1))) shouldEqual Some(1)
    m.get(Some(List("a"))) shouldEqual Some("b")

    val m2 = m.-(Some(List(1)))
    m2.get(Some(List(1))) shouldEqual None
    m2.get(Some(List("a"))) shouldEqual Some("b")

    val m3 = m2.-(Some(List("b"))) // Nonexistent element.
    m3.get(Some(List(1))) shouldEqual None
    m3.get(Some(List("a"))) shouldEqual Some("b")

    val m4 = m2.-(Some(List("a"))) // After this, the map is empty.
    m4.get(Some(List(1))) shouldEqual None
    m4.get(Some(List("a"))) shouldEqual None
    m4.isEmpty shouldEqual true
    m4.size shouldEqual 0
  }

  it should "fetch entries with different type parameters but the same values" in {
    val key1: Option[List[Int]] = None
    val key2: Option[List[String]] = None
    val m = HetTMap[Foo, Bar]()
      .updated(key1, 1)
      .updated(key2, "b")

    m.size shouldEqual 2
    m.get(Some(List(1))) shouldEqual None
    m.get(None) shouldEqual None // Wrong type.
    m.get(key1) shouldEqual Some(1)
    m.get(key2) shouldEqual Some("b")

    val m2 = m.-(key1)
    m2.get(key1) shouldEqual None
    m2.get(key2) shouldEqual Some("b")
  }
}
