package asserts.diff

sealed trait DiffForPrototype[T] {
  def compare(l: T, r: T): DiffResult
}

case class ObjDiffForPrototype[T](name: String,
                                  fields: List[FieldDiffPrototype[T, _]],
                                  comparator: Comparator[T, ObjDiffForPrototype[T]])
    extends DiffForPrototype[T] {
  override def compare(l: T, r: T): DiffResult = {
    comparator.compare(l, r, this)
  }
}

case class FieldDiffPrototype[T, U](name: String, accessChild: T => U, newDiffFor: DiffForPrototype[U])

case class ValueDiffPrototype[T](comparator: Comparator[T, ValueDiffPrototype[T]]) extends DiffForPrototype[T] {
  override def compare(l: T, r: T): DiffResult = {
    comparator.compare(l, r, this)
  }
}

trait Comparator[T, D <: DiffForPrototype[T]] {
  def compare(l: T, r: T, newDiffFor: D): DiffResult
}

object Comparator {
  def ignoreValue[T]: Comparator[T, ValueDiffPrototype[T]] = (_: T, _: T, _: ValueDiffPrototype[T]) => Ignored
  def ignoreObj[T]: Comparator[T, ObjDiffForPrototype[T]] = (_: T, _: T, _: ObjDiffForPrototype[T]) => Ignored
}

class ObjectComparator[T] extends Comparator[T, ObjDiffForPrototype[T]] {
  override def compare(l: T, r: T, obj: ObjDiffForPrototype[T]): DiffResult = {
    val partResults = obj.fields.map {
      case FieldDiffPrototype(fName, accessor, differ) =>
        fName -> (differ match {
          case d @ ObjDiffForPrototype(_, _, c) => c.compare(accessor(l), accessor(r), d)
          case d @ ValueDiffPrototype(c)        => c.compare(accessor(l), accessor(r), d)
        })
    }.toMap
    if (partResults.values.forall(_.isIdentical)) {
      Identical(l)
    } else {
      DiffResultObject(obj.name, partResults)
    }
  }
}

class ValueComparator[T] extends Comparator[T, ValueDiffPrototype[T]] {
  override def compare(l: T, r: T, newDiffFor: ValueDiffPrototype[T]): DiffResult = {
    if (l.toString == r.toString) {
      Identical(l)
    } else {
      DiffResultValue(l, r)
    }
  }
}
