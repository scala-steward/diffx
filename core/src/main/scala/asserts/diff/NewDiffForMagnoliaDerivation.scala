package asserts.diff

import magnolia._

import scala.language.experimental.macros

trait NewDiffForMagnoliaDerivation {
  type Typeclass[T] = DiffForPrototype[T]

  def combine[T](ctx: CaseClass[DiffForPrototype, T])(
      implicit comparator: Comparator[T, ObjDiffForPrototype[T]]): DiffForPrototype[T] = {
    val fields = ctx.parameters.map { p =>
      new FieldDiffPrototype[T, p.PType](p.label, { t: T =>
        p.dereference(t)
      }, p.typeclass)
    }
    new ObjDiffForPrototype[T](ctx.typeName.short, fields.toList, comparator)
  }

  def dispatch[T](ctx: SealedTrait[DiffForPrototype, T]): DiffForPrototype[T] = {
    ???
  }

  implicit def gen[T]: DiffForPrototype[T] = macro Magnolia.gen[T]

  implicit def comparatorForValue[T]: Comparator[T, ValueDiffPrototype[T]] = new ValueComparator[T]
  implicit def comparatorForObjects[T]: Comparator[T, ObjDiffForPrototype[T]] = new ObjectComparator[T]
}
