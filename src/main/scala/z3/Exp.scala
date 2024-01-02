package z3

import com.microsoft.z3.Native
import GlobalState.{gcptr => gcptr}

/** A Z3 expression.
 *  Users should construct expressions using the factory functions
 *  in [[z3]] rather than instantiate this class directly.
 **/
class Exp private[z3] (private[z3] val ptr: Long) {
  /** Returns the sort of this expression. */
  lazy val sort: Sort = Sort.create(Native.getSort(gcptr, ptr))

  override def toString: String = Native.astToString(gcptr, ptr)
}
