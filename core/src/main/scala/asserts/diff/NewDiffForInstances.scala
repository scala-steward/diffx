package asserts.diff

import java.time.Instant

trait NewDiffForInstances extends NewDiffForMagnoliaDerivation {

  implicit def diffForString2: NewDiffFor[String] = new ValueDiff[String](Strategy.Compare)
  implicit def diffForInt2: NewDiffFor[Int] = new ValueDiff[Int](Strategy.Compare)
  implicit def diffForInstant2: NewDiffFor[Instant] = new ValueDiff[Instant](Strategy.Compare)
}
