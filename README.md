# z3-scala-api

API for using Microsoft's Z3 SMT solver from Scala.

This API offers a better user experience for Scala programmers than the Java bindings that come with Z3 by default. The tradeoff is deviation from the Java and C APIs (in places). If you just want to call the C API functions from Scala, use the Java bindings.

Specific differences between the Scala API and the Java bindings:
* More "convenience functions" (e.g., `intConst("x")` instead of `const("x", intSort)`).
* Less rich types for expressions (e.g., `Exp` rather than `Exp[BoolSort]`).
* Support for defined functions and generic datatypes, both of which exist in SMTLib2 but not Z3 native.
* More fleshed-out support for datatypes, including generic datatypes.

Example:

```scala
import z3._
import z3.OperatorImplicits._

object Main extends Job(
  libz3_path="/full/path/to/libz3.so",
  params = newParams.timeout(10000).smtlib2_log("output.smt")
) {
  def main(args: Array[String]): Unit = {
    implicit val j: Job = this

    val x = intConst("x")
    val y = intConst("y")

    checkSat(
      3 * x + 5 * y === 49,
      0 <= x,
      x <= y
    ) match {
      case UNSAT => println("UNSAT")
      case UNKNOWN => println("UNKNOWN")
      case SAT(Model(sorts, constants, functions)) =>
        println(constants("x"))  // ~> 3
        println(constants("y"))  // ~> 8
    }

    close()
  }
}
```


TODO:
* SetSort (extends ArraySort)
* Sequences
* EnumSort (extends DatatypeSort)
* List sort (is it a built-in datatype?)
* Optimizers?
* Fixedpoint solver?
* Lambda expressions?
* Patterns?
* Regular expressions?
* Relations?
* Proofs, goals, and tactics?


## Resources

* [Z3 Source Code](https://github.com/Z3Prover/z3)
* [Z3 C API](https://z3prover.github.io/api/html/group__capi.html) - Documentation on the C API functions for Z3. The Java Native interface in the Z3 source code is nearly identical to the C interface.
* [ScalaZ3](https://github.com/epfl-lara/ScalaZ3) - Another set of Scala bindings for Z3 by EPFL-LARA.