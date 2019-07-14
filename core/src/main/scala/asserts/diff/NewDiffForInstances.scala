package asserts.diff

import java.time.Instant

trait NewDiffForInstances extends NewDiffForMagnoliaDerivation {

  implicit def diffForString2(implicit st: Strategy[String]): NewDiffFor[String] = new ValueDiff[String](st)
  implicit def diffForInt2(implicit st: Strategy[Int]): NewDiffFor[Int] = new ValueDiff[Int](st)
  implicit def diffForInstant2(implicit st: Strategy[Instant]): NewDiffFor[Instant] = new ValueDiff[Instant](st)

  implicit def exported[T](implicit ev: NewDiffFor[T]): NewExported[T] = NewExported(ev)

  implicit def diffForOption[T: NewDiffFor: Strategy]: NewDiffFor[Option[T]] =
    new ValueDiff[Option[T]](implicitly[Strategy[T]]) {
      override def compare(left: Option[T], right: Option[T]): DiffResult = {
        (left, right) match {
          case (Some(l), Some(r)) => implicitly[NewDiffFor[T]].compare(l, r)
          case (None, None)       => Identical(None)
          case (l, r)             => DiffResultValue(l, r)
        }
      }
    }
}

case class NewExported[T](v: NewDiffFor[T])
