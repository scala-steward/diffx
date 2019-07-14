package asserts.diff

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class CTest extends FlatSpec with Matchers with NewDiffForInstances {

  private val instant: Instant = Instant.now()
  val p1 = Person("kasper", 22, instant)
  val p2 = Person("kasper", 11, instant)

  "C" should "calculate diff for simple value" in {
    compare(1, 2) shouldBe DiffResultValue(1, 2)
    compare(1, 1) shouldBe Identical(1)
  }

  it should "calculate diff for product types" in {
    implicit val st1: Strategy[String] = Strategy.Ignore[String]()
    compare(p1, p2) shouldBe DiffResultObject(
      "Person",
      Map("name" -> Identical("kasper"), "age" -> DiffResultValue(22, 11), "in" -> Identical(instant)))
  }

  it should "calculate identity for product types" in {
    compare(p1, p1) shouldBe Identical(p1)
  }

  private def compare[T: NewDiffFor](t1: T, t2: T) = implicitly[NewDiffFor[T]].compare(t1, t2)
}
