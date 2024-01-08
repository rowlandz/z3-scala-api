package z3

import com.microsoft.z3.{Native, Z3Exception}
import com.microsoft.z3.enumerations.Z3_sort_kind

/** A Z3 sort (what most programming languages call a ''type'').
 *  See subclasses for more information about particular sorts. */
sealed abstract class Sort private[z3] (job: Job, private[z3] val ptr: Long) {
  lazy val name: String = Native.getSymbolString(job.cptr, Native.getSortName(job.cptr, ptr))

  def equalsSort(other: Sort): Boolean = Native.isEqSort(job.cptr, ptr, other.ptr)

  override def toString: String = Native.sortToString(job.cptr, ptr)
}

/** The sort that contains the boolean values `true` and `false`. The sort itself can be
 *  constructed with [[Job.boolSort]] and its values with [[Job.bool]]. */
final class BoolSort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr)

/** The sort that contains the integers.
 *  The sort itself can be constructed with [[Job.intSort]] and its values with `z3.int`. */
final class IntSort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr)

/** The sort that contains the real numbers.
 *  The sort itself can be constructed with [[Job.realSort]] and its values with `z3.real`. */
final class RealSort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr)

/** A sequence of `size` bits representing an integer or bit fields. Bit-vectors support
 *  arithmetic, comparison, and bitwise operations. The same sort represents both signed
 *  and unsigned integers, and the interpretation depends on the operations used. For
 *  example, the unsigned less-than expression `bvult(0x00, 0xff)` evaluates to `true`
 *  while the signed expression `bvslt(0x00, 0xff)` evaluates to `false`. */
final class BitVectorSort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr) {
  lazy val size: Int = Native.getBvSortSize(job.cptr, ptr)
}

/** A non-recursive algebraic data type.
 *
 *  Only one data type of a given name can exist at once. If a second data type is created with the same
 *  name as an existing data type, the second data type will overwrite the first.
 *  {{{
 *    import z3._
 *
 *    val maybeInt = datatypeSort("MaybeInt",
 *      constructor("nothing", "isNothing"),
 *      constructor("just", "isJust", field("x", intSort))
 *    )
 *
 *    val List(nothing, just) = maybeInt.constructors
 *    val List(isNothing, isJust) = maybeInt.recognizers
 *    val x = maybeInt.getAccessor("just", "x")
 *
 *    val safeDivision = new DefinedFunc("safeDivision", List("x" -> intSort, "y" -> intSort), maybeInt,
 *      ite(equ(intConst("y"), int(0)),
 *        nothing(),
 *        just(div(intConst("x"), intConst("y"))))
 *    )
 *  }}}
 */
class DatatypeSort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr) {

  /** List of constructors for this data type. The order of the constructors is the same as in the type definition. */
  lazy val constructors: List[FuncDecl] = (0 until Native.getDatatypeSortNumConstructors(job.cptr, ptr)).map { i =>
    new FuncDecl(job, Native.getDatatypeSortConstructor(job.cptr, ptr, i))
  }.toList

  /** List of recognizers for this data type. The order of the recognizers is the same as in the type definition. */
  lazy val recognizers: List[FuncDecl] = (0 until Native.getDatatypeSortNumConstructors(job.cptr, ptr)).map { i =>
    new FuncDecl(job, Native.getDatatypeSortRecognizer(job.cptr, ptr, i))
  }.toList

  /** Returns the constructor for this data type with name `name`. */
  def getConstructor(name: String): FuncDecl = job.datatypeConstructorNames(this.name).indexOf(name) match {
    case i if i >= 0 => new FuncDecl(job, Native.getDatatypeSortConstructor(job.cptr, ptr, i))
    case _ => throw new Z3Exception(s"Constructor $name does not exist for this type")
  }

  /** Returns the recognizer for this data type with name `name`. */
  def getRecognizer(name: String): FuncDecl = job.datatypeRecognizerNames(this.name).indexOf(name) match {
    case i if i >= 0 => new FuncDecl(job, Native.getDatatypeSortRecognizer(job.cptr, ptr, i))
    case _ => throw new Z3Exception(s"Recognizer $name does not exist for this type")
  }

  /** Returns the accessor function associated with `field` of `constructor`. */
  def getAccessor(constructor: String, field: String): FuncDecl = {
    val constructorIndex = job.datatypeConstructorNames(this.name).indexOf(constructor)
    val accessorIndex = job.datatypeFields(this.name)(constructorIndex).indexWhere(_.name == field)
    new FuncDecl(job, Native.getDatatypeSortConstructorAccessor(job.cptr, ptr, constructorIndex, accessorIndex))
  }

  /** Returns the SMTLib2  `(declare-datatype ...)` type declaration string. */
  def smt2string: String = {
    val smt2constructors = job.datatypeConstructorNames(name).zip(job.datatypeFields(name)).map {
      case (c, fs) =>
        val fieldList = fs.map(f => s"(${f.name} ${f.sort})").mkString(" ")
        s"($c $fieldList)"
    }.mkString("\n  ")

    s"""(declare-datatype $name (
       |  $smt2constructors
       |))""".stripMargin
  }
}

class Constructor private[z3] (private[z3] val ptr: Long,
                               private[z3] val constructorName: String,
                               private[z3] val recognizerName: String,
                               private[z3] val fields: Array[Field])

class Field(val name: String, val sort: Sort)

/** A data type with a single constructor. Also called a ''record''.
 *
 *  @example
 *  {{{
 *    import z3._
 *
 *    val person = structSort("Person", "mkPerson", "isPerson",
 *      field("male", boolSort),
 *      field("age", intSort),
 *      field("height", realSort)
 *    )
 *
 *    val List(male, age, height) = person.projectors
 *  }}}
 */
class StructSort private[z3] (job: Job, ptr: Long) extends DatatypeSort(job, ptr) {
  /** List of field projector functions for this struct type. A projector function extracts a field from a struct. */
  lazy val projectors: List[FuncDecl] = job.datatypeFields(this.name)(0).indices.map { i =>
    new FuncDecl(job, Native.getDatatypeSortConstructorAccessor(job.cptr, ptr, 0, i))
  }.toList

  /** Returns the projector function for `field`. */
  def getProjector(field: String): FuncDecl = {
    val accessorIndex = job.datatypeFields(this.name)(0).indexWhere(_.name == field)
    new FuncDecl(job, Native.getDatatypeSortConstructorAccessor(job.cptr, ptr, 0, accessorIndex))
  }
}


class ArraySort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr)

// TODO support this
class SetSort private[z3] (job: Job, ptr: Long) extends ArraySort(job, ptr)

/** An arbitrary sort. If a satisfiability check results in [[SAT]], the generated
 *  [[Model]] will contain interpretations for any uninterpreted sorts used in
 *  the assertions. An interpretation (universe) for an uninterpreted sort is a finite
 *  set of distinct abstract values. */
class UninterpretedSort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr)

/** A sort representing values of the IEEE-754 floating-point standard. */
final class FPSort private[z3] (job: Job, ptr: Long) extends Sort(job, ptr)

private[z3] object Sort {
  def create(job: Job, ptr: Long): Sort = Z3_sort_kind.fromInt(Native.getSortKind(job.cptr, ptr)) match {
    case Z3_sort_kind.Z3_UNINTERPRETED_SORT => new UninterpretedSort(job, ptr)
    case Z3_sort_kind.Z3_BOOL_SORT => new BoolSort(job, ptr)
    case Z3_sort_kind.Z3_INT_SORT => new IntSort(job, ptr)
    case Z3_sort_kind.Z3_REAL_SORT => new RealSort(job, ptr)
    case Z3_sort_kind.Z3_BV_SORT => new BitVectorSort(job, ptr)
    case Z3_sort_kind.Z3_ARRAY_SORT => new ArraySort(job, ptr)
    case Z3_sort_kind.Z3_DATATYPE_SORT => new DatatypeSort(job, ptr)
    case Z3_sort_kind.Z3_RELATION_SORT => ???
    case Z3_sort_kind.Z3_FINITE_DOMAIN_SORT => ???
    case Z3_sort_kind.Z3_FLOATING_POINT_SORT => new FPSort(job, ptr)
    case Z3_sort_kind.Z3_ROUNDING_MODE_SORT => throw new Z3Exception("Z3 Scala error: attempt to create a sort of kind Z3_ROUNDING_MODE_SORT")
    case Z3_sort_kind.Z3_SEQ_SORT => ???
    case Z3_sort_kind.Z3_RE_SORT => ???
    case Z3_sort_kind.Z3_CHAR_SORT => ???
    case Z3_sort_kind.Z3_UNKNOWN_SORT => ???
  }
}