package z3

import com.microsoft.z3.Z3Exception

/** Definitions for building expressions using operators.
 *  @example
 *  {{{
 *    import z3.OperatorImplicits._
 *
 *    val x = intConst("x")
 *    val y = intConst("y")
 *
 *    println(checkSat(
 *      x > 0,
 *      y > 0,
 *      2*x + 3*y === 29
 *    ))
 *  }}}
 **/
object OperatorImplicits {
  implicit class ExpExp(e1: Exp) {
    /** Alias for [[z3.add]]`(e1, e2)` */
    @inline final def +(e2: Exp): Exp = add(e1, e2)
    /** Alias for [[z3.sub]]`(e1, e2)` */
    @inline final def -(e2: Exp): Exp = sub(e1, e2)
    /** Alias for [[z3.mul]]`(e1, e2)` */
    @inline final def *(e2: Exp): Exp = mul(e1, e2)
    /** Alias for [[z3.div]]`(e1, e2)` */
    @inline final def /(e2: Exp): Exp = div(e1, e2)

    /** Alias for [[z3.equ]]`(e1, e2)` */
    @inline final def ===(e2: Exp): Exp = equ(e1, e2)
    /** Alias for [[z3.distinct]]`(e1, e2)` */
    @inline final def !==(e2: Exp): Exp = distinct(e1, e2)

    /** Alias for [[z3.lt]]`(e1, e2)` */
    @inline final def <(e2: Exp): Exp = lt(e1, e2)
    /** Alias for [[z3.le]]`(e1, e2)` */
    @inline final def <=(e2: Exp): Exp = le(e1, e2)
    /** Alias for [[z3.gt]]`(e1, e2)` */
    @inline final def >(e2: Exp): Exp = gt(e1, e2)
    /** Alias for [[z3.ge]]`(e1, e2)` */
    @inline final def >=(e2: Exp): Exp = ge(e1, e2)
  }

  implicit class IntExp(v: Int) {
    @inline final def +(e: Exp): Exp = add(literal(v, e.sort), e)
    @inline final def *(e: Exp): Exp = mul(literal(v, e.sort), e)

    @inline final def ===(e: Exp): Exp = equ(literal(v, e.sort), e)
    @inline final def !==(e: Exp): Exp = distinct(literal(v, e.sort), e)

    @inline final def <(e: Exp): Exp = lt(literal(v, e.sort), e)
    @inline final def <=(e: Exp): Exp = le(literal(v, e.sort), e)
    @inline final def >(e: Exp): Exp = gt(literal(v, e.sort), e)
    @inline final def >=(e: Exp): Exp = ge(literal(v, e.sort), e)
  }

  implicit class ExpInt(e: Exp) {
    @inline final def +(v: Int): Exp = add(e, literal(v, e.sort))
    @inline final def *(v: Int): Exp = mul(e, literal(v, e.sort))

    @inline final def ===(v: Int): Exp = equ(e, literal(v, e.sort))
    @inline final def !==(v: Int): Exp = distinct(e, literal(v, e.sort))

    @inline final def <(v: Int): Exp = lt(e, literal(v, e.sort))
    @inline final def <=(v: Int): Exp = le(e, literal(v, e.sort))
    @inline final def >(v: Int): Exp = gt(e, literal(v, e.sort))
    @inline final def >=(v: Int): Exp = ge(e, literal(v, e.sort))
  }
}