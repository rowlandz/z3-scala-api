package z3

/** Definitions for building expressions using operators.
 *  @example
 *  {{{
 *    import z3._
 *    import z3.OperatorImplicits._
 *
 *    object Main extends Job("/full/path/to/libz3.so") {
 *      def main(args: Array[String]): Unit = {
 *        implicit val j: Job = this
 *
 *        val x = realConst("x")
 *        val y = realConst("y")
 *
 *        println(checkSat(
 *          x > 0,
 *          y > 0,
 *          2*x + 3*y === 29
 *        ))
 *
 *        close()
 *      }
 *    }
 *  }}}
 **/
object OperatorImplicits {
  implicit class ExpExp(e1: Exp) {
    /** Alias for [[Job.add]]`(e1, e2)` */
    @inline final def +(e2: Exp)(implicit job: Job): Exp = job.add(e1, e2)
    /** Alias for [[Job.sub]]`(e1, e2)` */
    @inline final def -(e2: Exp)(implicit job: Job): Exp = job.sub(e1, e2)
    /** Alias for [[Job.mul]]`(e1, e2)` */
    @inline final def *(e2: Exp)(implicit job: Job): Exp = job.mul(e1, e2)
    /** Alias for [[Job.div]]`(e1, e2)` */
    @inline final def /(e2: Exp)(implicit job: Job): Exp = job.div(e1, e2)

    /** Alias for [[Job.equ]]`(e1, e2)` */
    @inline final def ===(e2: Exp)(implicit job: Job): Exp = job.equ(e1, e2)
    /** Alias for [[Job.distinct]]`(e1, e2)` */
    @inline final def !==(e2: Exp)(implicit job: Job): Exp = job.distinct(e1, e2)

    /** Alias for [[Job.lt]]`(e1, e2)` */
    @inline final def <(e2: Exp)(implicit job: Job): Exp = job.lt(e1, e2)
    /** Alias for [[Job.le]]`(e1, e2)` */
    @inline final def <=(e2: Exp)(implicit job: Job): Exp = job.le(e1, e2)
    /** Alias for [[Job.gt]]`(e1, e2)` */
    @inline final def >(e2: Exp)(implicit job: Job): Exp = job.gt(e1, e2)
    /** Alias for [[Job.ge]]`(e1, e2)` */
    @inline final def >=(e2: Exp)(implicit job: Job): Exp = job.ge(e1, e2)
  }

  implicit class IntExp(v: Int) {
    @inline final def +(e: Exp)(implicit job: Job): Exp = job.add(job.literal(v, e.sort), e)
    @inline final def *(e: Exp)(implicit job: Job): Exp = job.mul(job.literal(v, e.sort), e)

    @inline final def ===(e: Exp)(implicit job: Job): Exp = job.equ(job.literal(v, e.sort), e)
    @inline final def !==(e: Exp)(implicit job: Job): Exp = job.distinct(job.literal(v, e.sort), e)

    @inline final def <(e: Exp)(implicit job: Job): Exp = job.lt(job.literal(v, e.sort), e)
    @inline final def <=(e: Exp)(implicit job: Job): Exp = job.le(job.literal(v, e.sort), e)
    @inline final def >(e: Exp)(implicit job: Job): Exp = job.gt(job.literal(v, e.sort), e)
    @inline final def >=(e: Exp)(implicit job: Job): Exp = job.ge(job.literal(v, e.sort), e)
  }

  implicit class ExpInt(e: Exp) {
    @inline final def +(v: Int)(implicit job: Job): Exp = job.add(e, job.literal(v, e.sort))
    @inline final def *(v: Int)(implicit job: Job): Exp = job.mul(e, job.literal(v, e.sort))

    @inline final def ===(v: Int)(implicit job: Job): Exp = job.equ(e, job.literal(v, e.sort))
    @inline final def !==(v: Int)(implicit job: Job): Exp = job.distinct(e, job.literal(v, e.sort))

    @inline final def <(v: Int)(implicit job: Job): Exp = job.lt(e, job.literal(v, e.sort))
    @inline final def <=(v: Int)(implicit job: Job): Exp = job.le(e, job.literal(v, e.sort))
    @inline final def >(v: Int)(implicit job: Job): Exp = job.gt(e, job.literal(v, e.sort))
    @inline final def >=(v: Int)(implicit job: Job): Exp = job.ge(e, job.literal(v, e.sort))
  }
}