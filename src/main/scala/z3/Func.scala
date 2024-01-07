package z3

import com.microsoft.z3.Native

/** A function declaration, which contains the name and type signature of an uninterpreted function.
 *  Function declarations should be constructed using [[Job.funcDecl]] rather than calling the
 *  constructor directly. */
class FuncDecl private[z3] (job: Job, private[z3] val ptr: Long) {
  /** Creates a function application expression representing this function applied to `args`. */
  @inline final def apply(args: Exp*): Exp = new Exp(job, Native.mkApp(job.cptr, ptr, args.length, args.map(_.ptr).toArray))

  lazy val name: String = Native.getSymbolString(job.cptr, Native.getDeclName(job.cptr, ptr))

  override def toString: String = Native.funcDeclToString(job.cptr, ptr)
}

/** An interpreted function; a function with a definition.
 *  @constructor Creates a new interpreted function
 *  @param name name of the function
 *  @param params list of parameter names and sorts
 *  @param range return type of this function
 *  @param body function body
 **/
class DefinedFunc private[z3] (job: Job, val name: String, val params: List[(String, Sort)], val range: Sort, val body: Exp) {
  /** Creates an expression equivalent to this function applied to `args` by substituting `args`
   *  into this function's body. */
  @inline final def apply(args: Exp*): Exp = {
    if (params.length != args.length)
      throw new Exception("Invalid function application: incorrect number of args")
    val varsToReplace = params.map { case (param, sort) => Native.mkConst(job.cptr, Native.mkStringSymbol(job.cptr, param), sort.ptr) }
    val ePtr = Native.substitute(job.cptr, body.ptr, args.length, varsToReplace.toArray, args.map(_.ptr).toArray)
    new Exp(job, ePtr)
  }

  /** Returns the SMTLib2  `(define-fun ...)` function definition string. */
  def smt2string: String = {
    val paramsString = params.map(p => s"(${p._1} ${p._2})").mkString(" ")

    s"""(define-fun $name ($paramsString) $range
       |  $body
       |)""".stripMargin
  }

  @inline override final def toString: String = smt2string
}