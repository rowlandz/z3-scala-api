# z3-scala-api
API for using Microsoft's Z3 SMT solver using Scala

Design Goals:
* Less rich types for expressions
* Single global Z3 context and solver
* Support for defined functions and generic datatypes, both of which
  exist in SMTLib2 but not Z3 native.

TODO:
* Bitvectors
* IEEE floating point
* SetSort (extends ArraySort)
* Quantifiers
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


Resources:
* [Z3 C API](https://z3prover.github.io/api/html/group__capi.html) - Documentation on the C API functions for Z3. The Java Native interface in the Z3 source code is nearly identical to the C interface.