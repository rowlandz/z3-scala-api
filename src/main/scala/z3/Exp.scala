package z3

import com.microsoft.z3.Native

/** A Z3 expression.
 *  Users should construct expressions using the factory functions
 *  in [[Job]] rather than instantiate this class directly.
 **/
class Exp private[z3] (job: Job, private[z3] val ptr: Long) {
  /** Returns the sort of this expression. */
  lazy val sort: Sort = Sort.create(job, Native.getSort(job.cptr, ptr))

  override def toString: String = Native.astToString(job.cptr, ptr)
}
