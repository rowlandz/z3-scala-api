package z3

import com.microsoft.z3.Z3Exception

/** The template of a data type whose type parameters can be filled in to create a [[DatatypeSort]].
 *
 *  @example
 *  {{{
 *    import z3._
 *
 *    val maybe = genericDatatype("Maybe", List("T"),
 *      genericConstructor("nothing", "isNothing"),
 *      genericConstructor("just", "isJust", genericField("x", "T"))
 *    )
 *
 *    val maybeInt: DatatypeSort = maybe.instantiate("MaybeInt", intSort)
 *    val maybeReal: DatatypeSort = maybe.instantiate("MaybeReal", realSort)
 *  }}}
 *
 *  @constructor Creates a new generic data type
 *  @param name name of the generic data type
 *  @param tparams type parameters
 *  @param cons constructors
 */
class GenericDatatype(job: Job, val name: String, val tparams: List[String], val cons: GenericConstructor*) {

  /** Creates a data type by replacing all type parameters in this generic data type with the supplied type arguments.
   *  @param newName name of the newly created data type
   *  @param targs type arguments */
  def instantiate(newName: String, targs: Sort*): DatatypeSort = {
    if (targs.length != tparams.length)
      throw new Z3Exception(s"Cannot instantiate generic datatype $name: incorrect number of type arguments")
    val tparamValues: Map[String, Sort] = tparams.zip(targs).toMap
    job.datatypeSort(newName, cons.map(_.instantiate(tparamValues)):_*)
  }

  /** Returns the SMTLib2  `(declare-datatype ...)` type declaration string. */
  def smt2string: String = {
    val smt2constructors = cons.map { c =>
      val fieldList =  c.fields.map { f =>
        val sortOrTparamString = f.sortOrTparam match {
          case Left(sort) => sort.toString
          case Right(tparam) => tparam
        }
        s"(${f.name} $sortOrTparamString)"
      }.mkString(" ")
      s"(${c.name} $fieldList)"
    }.mkString("\n  ")

    val tparamsString = tparams.mkString(" ")

    s"""(declare-datatype $name (par ($tparamsString) (
       |  $smt2constructors
       |)))""".stripMargin
  }

  @inline override final def toString: String = smt2string
}

class GenericConstructor(job: Job, val name: String, val recognizer: String, val fields: GenericField*) {
  private[z3] def instantiate(tparamValues: Map[String, Sort]): Constructor =
    job.constructor(name, recognizer, fields.map(_.instantiate(tparamValues)):_*)
}

case class GenericField(name: String, sortOrTparam: Either[Sort, String]) {
  private[z3] def instantiate(tparamValues: Map[String, Sort]): Field = sortOrTparam match {
    case Left(sort) => new Field(name, sort)
    case Right(tparam) => new Field(name, tparamValues(tparam))
  }
}