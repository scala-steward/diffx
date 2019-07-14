package asserts.diff

object QuicklensAdapter {

  implicit class ModifyPimp[T](t: DiffFor[T]) {

    /**
      * Create an object allowing modifying the given (deeply nested) field accessible in a `case class` hierarchy
      * via `path` on the given `obj`.
      *
      * All modifications are side-effect free and create copies of the original objects.
      *
      * You can use `.each` to traverse options, lists, etc.
      */
    def modify[U](path: T => U): T => T = macro QuicklensMacrosAdapter.modifyPimp_impl[T, U]
  }
  case class PathModify[T, U](obj: T, doModify: (T, U => U) => T) {

    /**
      * Transform the value of the field(s) using the given function.
      *
      * @return A copy of the root object with the (deeply nested) field(s) modified.
      */
    def using(mod: U => U): T = doModify(obj, mod)

    /**
      * Transform the value of the field(s) using the given function, if the condition is true. Otherwise, returns the
      * original object unchanged.
      *
      * @return A copy of the root object with the (deeply nested) field(s) modified, if `condition` is true.
      */
    def usingIf(condition: Boolean)(mod: U => U): T = if (condition) doModify(obj, mod) else obj

    /**
      * Set the value of the field(s) to a new value.
      *
      * @return A copy of the root object with the (deeply nested) field(s) set to the new value.
      */
    def setTo(v: U): T = doModify(obj, _ => v)

    /**
      * Set the value of the field(s) to a new value, if it is defined. Otherwise, returns the original object
      * unchanged.
      *
      * @return A copy of the root object with the (deeply nested) field(s) set to the new value, if it is defined.
      */
    def setToIfDefined(v: Option[U]): T = v.fold(obj)(setTo)

    /**
      * Set the value of the field(s) to a new value, if the condition is true. Otherwise, returns the original object
      * unchanged.
      *
      * @return A copy of the root object with the (deeply nested) field(s) set to the new value, if `condition` is
      *         true.
      */
    def setToIf(condition: Boolean)(v: => U): T = if (condition) setTo(v) else obj
  }
}
