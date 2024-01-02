package z3

/** The result from the a satisfiability check [[checkSat(assertions:Array*]]. */
sealed trait Status

/** The status that indicates unsatisfiability. */
case object UNSAT extends Status

/** The status indicating that Z3 failed to determine satisfiability. */
case object UNKNOWN extends Status

/** The status that indicates satisfiability. */
case class SAT(model: Model) extends Status {
  override def toString: String = "SAT\n" + model.toString
}