package asserts.diff

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object QuicklensMacrosAdapter {
  private val ShapeInfo = "Path must have shape: _.field1.field2.each.field3.(...)"

  /**
    * a.modify(_.b.c) => new PathMod(a, (A, F) => A.copy(b = A.b.copy(c = F(A.b.c))))
    */
  def modifyPimp_impl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: blackbox.Context)(
      path: c.Expr[T => U]
  ): c.Tree = {
    import c.universe._

    case class TermPathElement(term: c.TermName)

    def collectPathElements(tree: c.Tree, acc: List[TermPathElement]): List[TermPathElement] = {
      tree match {
        case q"$parent.$child" => collectPathElements(parent, TermPathElement(child) :: acc)
        case t: Ident          => acc
      }
    }

    val list = path.tree match {
      case q"($args) => $pathBody" =>
        collectPathElements(pathBody, Nil)
    }

    val symbol = c.symbolOf[T]
    val t = weakTypeOf[T]

    println(list)

    q"""{t: $t => t }"""
  }
//  def modifyWrapped[T: c.WeakTypeTag, U: c.WeakTypeTag](c: blackbox.Context)(
//    modifications: c.Tree
//  ): c.Tree = {
//    import c.universe._
//
//    val wrappedValue = c.macroApplication match {
//      case Apply(TypeApply(Select(Apply(_, List(w)), _), _), _) => w
//      case _                                                    => c.abort(c.enclosingPosition, s"Unknown usage of ModifyPimp. Please file a bug.")
//    }
//
//    val valueAlias = TermName(c.freshName())
//
//    q"""{
//      val $valueAlias = $wrappedValue;
//      _root_.com.softwaremill.quicklens.PathModify(${Ident(valueAlias)}, $modifications)
//     }"""
//  }
//
//  /**
//    * Produce a modification for a single path.
//    */
//  private def modificationForPath[T: c.WeakTypeTag, U: c.WeakTypeTag](c: blackbox.Context)(
//    path: c.Expr[T => U]
//  ): c.Tree = {
//    import c.universe._
//
//    sealed trait PathAccess
//    case object DirectPathAccess extends PathAccess
//    case class SealedPathAccess(types: Set[Symbol]) extends PathAccess
//
//    sealed trait PathElement
//    case class TermPathElement(term: c.TermName, access: PathAccess, xargs: c.Tree*) extends PathElement
//    case class SubtypePathElement(subtype: c.Symbol) extends PathElement
//    case class FunctorPathElement(functor: c.Tree, method: c.TermName, xargs: c.Tree*) extends PathElement
//
//    /**
//      * Determine if the `.copy` method should be applied directly
//      * or through a match across all subclasses (for sealed traits).
//      */
//    def determinePathAccess(typeSymbol: Symbol) = {
//      def ifEmpty[A](set: Set[A], empty: => Set[A]) =
//        if (set.isEmpty) empty else set
//
//      def knownDirectSubclasses(sealedSymbol: ClassSymbol) = ifEmpty(
//        sealedSymbol.knownDirectSubclasses,
//        c.abort(
//          c.enclosingPosition,
//          s"""Could not find subclasses of sealed trait $sealedSymbol.
//             |You might need to ensure that it gets compiled before this invocation.
//             |See also: <https://issues.scala-lang.org/browse/SI-7046>.""".stripMargin
//        )
//      )
//
//      def expand(symbol: Symbol): Set[Symbol] =
//        Set(symbol)
//          .filter(_.isClass)
//          .map(_.asClass)
//          .map { s =>
//            s.typeSignature; s
//          } // see <https://issues.scala-lang.org/browse/SI-7755>
//          .filter(_.isSealed)
//          .flatMap(s => knownDirectSubclasses(s))
//          .flatMap(s => ifEmpty(expand(s), Set(s)))
//
//      val subclasses = expand(typeSymbol)
//      if (subclasses.isEmpty) DirectPathAccess else SealedPathAccess(subclasses)
//    }
//
//    /**
//      * _.a.b.each.c => List(TPE(a), TPE(b), FPE(functor, each/at/eachWhere, xargs), TPE(c))
//      */
//    @tailrec
//    def collectPathElements(tree: c.Tree, acc: List[PathElement]): List[PathElement] = {
//      def methodSupported(method: TermName) =
//        Seq("at", "eachWhere").contains(method.toString)
//      def typeSupported(quicklensType: c.Tree) =
//        Seq("QuicklensEach", "QuicklensAt", "QuicklensMapAt", "QuicklensWhen", "QuicklensEither")
//          .exists(quicklensType.toString.endsWith)
//      tree match {
//        case q"$parent.$child" =>
//          val access = determinePathAccess(parent.tpe.typeSymbol)
//          collectPathElements(parent, TermPathElement(child, access) :: acc)
//        case q"$tpname[..$_]($parent).when[$tp]" if typeSupported(tpname) =>
//          collectPathElements(parent, SubtypePathElement(tp.tpe.typeSymbol) :: acc)
//        case q"$parent.$method(..$xargs)" if methodSupported(method) =>
//          collectPathElements(parent, TermPathElement(method, DirectPathAccess, xargs: _*) :: acc)
//        case q"$tpname[..$_]($t)($f)" if typeSupported(tpname) =>
//          val newAcc = acc match {
//            // replace the term controlled by quicklens
//            case TermPathElement(term, _, xargs @ _*) :: rest => FunctorPathElement(f, term, xargs: _*) :: rest
//            case pathEl :: _ =>
//              c.abort(c.enclosingPosition, s"Invalid use of path element $pathEl. $ShapeInfo, got: ${path.tree}")
//          }
//          collectPathElements(t, newAcc)
//        case t: Ident => acc
//        case _        => c.abort(c.enclosingPosition, s"Unsupported path element. $ShapeInfo, got: $tree")
//      }
//    }
//
//    /**
//      * (x, List(TPE(c), TPE(b), FPE(functor, method, xargs), TPE(a))) => x.b.c
//      */
//    def generateSelects(rootPathEl: c.TermName, reversePathEls: List[PathElement]): c.Tree = {
//      @tailrec
//      def terms(els: List[PathElement], result: List[c.TermName]): List[c.TermName] =
//        els match {
//          case Nil                               => result
//          case TermPathElement(term, _) :: tail  => terms(tail, term :: result)
//          case SubtypePathElement(_) :: _        => result
//          case FunctorPathElement(_, _, _*) :: _ => result
//        }
//
//      @tailrec
//      def go(els: List[c.TermName], result: c.Tree): c.Tree =
//        els match {
//          case Nil => result
//          case pathEl :: tail =>
//            val select = q"$result.$pathEl"
//            go(tail, select)
//        }
//
//      go(terms(reversePathEls, Nil), Ident(rootPathEl))
//    }
//
//    /**
//      * (tree, DirectPathAccess) => f(tree)
//      *
//      * (tree, SealedPathAccess(Set(T1, T2, ...)) => tree match {
//      *   case x1: T1 => f(x1)
//      *   case x2: T2 => f(x2)
//      *   ...
//      * }
//      */
//    def generateAccess(tree: c.Tree, access: PathAccess)(f: c.Tree => c.Tree) = access match {
//      case DirectPathAccess => f(tree)
//      case SealedPathAccess(types) =>
//        val cases = types map { tp =>
//          val pat = TermName(c.freshName())
//          cq"$pat: $tp => ${f(Ident(pat))}"
//        }
//        q"$tree match { case ..$cases }"
//    }
//
//    /**
//      * (a, List(TPE(d), TPE(c), FPE(functor, method, xargs), TPE(b)), k) =>
//      *   (aa, aa.copy(b = functor.method(aa.b, xargs)(a => a.copy(c = a.c.copy(d = k)))
//      */
//    def generateCopies(
//                        rootPathEl: c.TermName,
//                        reversePathEls: List[PathElement],
//                        newVal: c.Tree
//                      ): (c.TermName, c.Tree) =
//      reversePathEls match {
//        case Nil => (rootPathEl, newVal)
//        case TermPathElement(pathEl, access) :: tail =>
//          val selectCurrVal = generateSelects(rootPathEl, tail)
//          val copy = generateAccess(selectCurrVal, access) { currVal =>
//            q"$currVal.copy($pathEl = $newVal)"
//          }
//          generateCopies(rootPathEl, tail, copy)
//        case SubtypePathElement(subtype) :: tail =>
//          val newRootPathEl = TermName(c.freshName())
//          val intactPathEl = TermName(c.freshName())
//          val selectCurrVal = generateSelects(newRootPathEl, tail)
//          val cases = Seq(
//            cq"$rootPathEl: $subtype => $newVal",
//            cq"$intactPathEl => ${Ident(intactPathEl)}"
//          )
//          val modifySubtype = q"$selectCurrVal match { case ..$cases }"
//          generateCopies(newRootPathEl, tail, modifySubtype)
//        case FunctorPathElement(functor, method, xargs @ _*) :: tail =>
//          val newRootPathEl = TermName(c.freshName())
//          // combine the selected path with variable args
//          val args = generateSelects(newRootPathEl, tail) :: xargs.toList
//          val rootPathElParamTree = ValDef(Modifiers(), rootPathEl, TypeTree(), EmptyTree)
//          val functorMap = q"$functor.$method(..$args)(($rootPathElParamTree) => $newVal)"
//          generateCopies(newRootPathEl, tail, functorMap)
//      }
//
//    //
//
//    val pathEls = path.tree match {
//      case q"($arg) => $pathBody" => collectPathElements(pathBody, Nil)
//      case _                      => c.abort(c.enclosingPosition, s"$ShapeInfo, got: ${path.tree}")
//    }
//
//    // the initial root object (the end-root object can be different if there are .each's on the way)
//    val initialRootPathEl = TermName(c.freshName())
//    val fn = TermName(c.freshName()) // the function that modifies the last path element
//
//    val reversePathEls = pathEls.reverse
//
//    // new value of the last path element is an invocation of $fn on the current last path element value
//    val select = generateSelects(initialRootPathEl, reversePathEls)
//    val mod = q"$fn($select)"
//
//    val (rootPathEl, copies) = generateCopies(initialRootPathEl, reversePathEls, mod)
//
//    val rootPathElParamTree = q"val $rootPathEl: ${weakTypeOf[T]}"
//    val fnParamTree = q"val $fn: (${weakTypeOf[U]} => ${weakTypeOf[U]})"
//
//    q"($rootPathElParamTree, $fnParamTree) => $copies"
//  }
}
