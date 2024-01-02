package z3

import com.microsoft.z3.Native
import z3.GlobalState.{gcptr => gcptr}

/** A function declaration, which contains the name and type signature of an uninterpreted function.
 *  Function declarations should be consructed using [[z3.funcDecl]] rather than calling the
 *  constructor directly. */
class FuncDecl private[z3] (private[z3] val ptr: Long) {
  /** Creates a function application expression representing this function applied to `args`. */
  @inline final def apply(args: Exp*): Exp = app(this, args:_*)

  lazy val name: String = Native.getSymbolString(gcptr, Native.getDeclName(gcptr, ptr))

  override def toString: String = Native.funcDeclToString(gcptr, ptr)
}

/** An interpreted function; a function with a definition.
 *  @constructor Creates a new interpreted function
 *  @param name name of the function
 *  @param params list of parameter names and sorts
 *  @param range return type of this function
 *  @param body function body
 **/
case class DefinedFunc(name: String, params: List[(String, Sort)], range: Sort, body: Exp) {
  /** Creates an expression equivalent to this function applied to `args` by substituting `args`
   *  into this function's body. */
  @inline final def apply(args: Exp*): Exp = app(this, args:_*)

  /** Returns the SMTLib2  `(define-fun ...)` function definition string. */
  def smt2string: String = {
    val paramsString = params.map(p => s"(${p._1} ${p._2})").mkString(" ")

    s"""(define-fun $name ($paramsString) $range
       |  $body
       |)""".stripMargin
  }

  @inline override final def toString: String = smt2string
}