package asserts.diff

import java.time.Instant

trait NewDiffForInstances extends NewDiffForMagnoliaDerivation {

  implicit def diffForString2(implicit st: Comparator[String, ValueDiffPrototype[String]]): DiffForPrototype[String] =
    new ValueDiffPrototype[String](st)
  implicit def diffForInt2(implicit st: Comparator[Int, ValueDiffPrototype[Int]]): DiffForPrototype[Int] =
    new ValueDiffPrototype[Int](st)
  implicit def diffForInstant2(
      implicit st: Comparator[Instant, ValueDiffPrototype[Instant]]): DiffForPrototype[Instant] =
    new ValueDiffPrototype[Instant](st)

  implicit def exported[T](implicit ev: DiffForPrototype[T]): NewExported[T] = NewExported(ev)
}

case class NewExported[T](v: DiffForPrototype[T])
