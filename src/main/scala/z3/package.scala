import com.microsoft.z3.{Native, Z3Exception}
import com.microsoft.z3.enumerations.Z3_lbool

/** A Scala interface to Z3.
 * @example
 * {{{
 *   def main(args: Array[String]): Unit = {
 *     import z3._
 *     startZ3("/absolute/path/to/libz3.so")
 *     println(checkSat(and(boolConst("p"), not(boolConst("p")))))
 *     stopZ3()
 *   }
 * }}}
 */
package object z3 {
  import GlobalState.{gcptr, gsptr}

  /** Must be called before using Z3.
   *
   *  Specifically:
   *  - Loads the `libz3.so` library.
   *  - Initializes and configures a Z3 context
   *  - Initializes and configures a Z3 solver
   *  - Processes Scala API configuration parameters
   *  @param libz3_path full path to the `libz3.so` library
   *  @param params configuration options
   *  @see [[Parameters]], [[stopZ3]]
   */
  def startZ3(libz3_path: String, params: Parameters = newParams): Unit = GlobalState.startZ3(libz3_path, params)

  /** Clears Z3. */
  def stopZ3(): Unit = GlobalState.stopZ3()

  /** Default configuration parameters for Z3.
   *  @see [[Parameters]] */
  val newParams: Parameters = new Parameters(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  /** The sort that contains the booleans. */
  lazy val boolSort: Sort = new BoolSort(Native.mkBoolSort(gcptr))

  /** The sort that contains the integers. */
  lazy val intSort: Sort = new IntSort(Native.mkIntSort(gcptr))

  /** The sort that contains the real numbers. */
  lazy val realSort: Sort = new RealSort(Native.mkRealSort(gcptr))

  /** The sort that contains unsigned or two's complement integers of `size` bits. */
  def bvSort(size: Int): BitVectorSort = new BitVectorSort(Native.mkBvSort(gcptr, size))

  /** Creates a new non-recursive algebraic datatype. */
  def datatypeSort(name: String, constructors: Constructor*): DatatypeSort = {
    if (!GlobalState.allowDatatypeOverwrite && GlobalState.datatypeConstructorNames.contains(name))
      throw new Z3Exception(s"Datatype named $name already exists.")

    GlobalState.datatypeConstructorNames += (name -> constructors.map(_.constructorName).toArray)
    GlobalState.datatypeRecognizerNames += (name -> constructors.map(_.recognizerName).toArray)
    GlobalState.datatypeFields += (name -> constructors.map(_.fields).toArray)

    new DatatypeSort(
      Native.mkDatatype(gcptr,
        Native.mkStringSymbol(gcptr, name),
        constructors.length,
        constructors.map(_.ptr).toArray
      )
    )
  }

  /** Creates a constructor for a [[datatypeSort]]. */
  def constructor(name: String, recognizer: String, fields: Field*): Constructor = {
    val fieldNamePtrs = fields.map(f => Native.mkStringSymbol(gcptr, f.name)).toArray
    val fieldSortPtrs = fields.map(_.sort.ptr).toArray

    new Constructor(
      Native.mkConstructor(gcptr,
        Native.mkStringSymbol(gcptr, name),
        Native.mkStringSymbol(gcptr, recognizer),
        fields.length,
        fieldNamePtrs,
        fieldSortPtrs,
        Array.fill(fields.length)(0)
      ),
      name,
      recognizer,
      fields.toArray
    )
  }

  /** Creates a field for a [[constructor]]. */
  @inline final def field(name: String, sort: Sort): Field = new Field(name, sort)

  /** Creates a new non-recursive struct type. */
  def structSort(name: String, constructor: String, recognizer: String, fields: Field*): StructSort = {
    if (!GlobalState.allowDatatypeOverwrite && GlobalState.datatypeConstructorNames.contains(name))
      throw new Z3Exception(s"Datatype named $name already exists.")

    GlobalState.datatypeConstructorNames += (name -> Array(constructor))
    GlobalState.datatypeRecognizerNames += (name -> Array(recognizer))
    GlobalState.datatypeFields += (name -> Array(fields.toArray))

    new StructSort(
      Native.mkDatatype(gcptr,
        Native.mkStringSymbol(gcptr, name),
        1,
        Array(z3.constructor(constructor, recognizer, fields:_*).ptr)
      )
    )
  }

  /** The array sort containing values of sort `range` indexed by `domain`. */
  def arraySort(domain: Sort, range: Sort): ArraySort =
    new ArraySort(Native.mkArraySort(gcptr, domain.ptr, range.ptr))

  def uninterpretedSort(name: String): UninterpretedSort =
    new UninterpretedSort(Native.mkUninterpretedSort(gcptr, Native.mkStringSymbol(gcptr, name)))

  /** Creates a new non-recursive generic data type. */
  @inline final def genericDatatype(name: String, tParams: List[String], constructors: GenericConstructor*): GenericDatatype =
    new GenericDatatype(name, tParams, constructors:_*)

  @inline final def genericConstructor(name: String, recognizer: String, fields: GenericField*): GenericConstructor =
    new GenericConstructor(name, recognizer, fields:_*)

  @inline final def genericField(name: String, sort: Sort): GenericField = GenericField(name, Left(sort))

  @inline final def genericField(name: String, tparam: String): GenericField = GenericField(name, Right(tparam))

  /** Creates a new function definition. When used in expressions, the body of a defined function is automatically
   *  substituted for the function call. */
  @inline final def definedFunc(name: String, params: List[(String, Sort)], range: Sort, body: Exp): DefinedFunc =
    DefinedFunc(name, params, range, body)

  /** Creates a boolean literal expression with sort [[boolSort]].
   *  @param b value of the literal */
  def bool(b: Boolean): Exp = new Exp(if (b) Native.mkTrue(gcptr) else Native.mkFalse(gcptr))

  /** Creates an integer literal expression with sort [[intSort]].
   *  @param v value of the literal
   **/
  def int(v: Int): Exp = new Exp(Native.mkInt(gcptr, v, intSort.ptr))

  /** Creates an integer literal expression with sort [[intSort]].
   *  @param v value of the literal */
  def int(v: Long): Exp = new Exp(Native.mkInt64(gcptr, v, intSort.ptr))

  /** Creates an integer literal expression with sort [[intSort]].
   *  @param s decimal string representing the value of the literal */
  def int(s: String): Exp = new Exp(Native.mkNumeral(gcptr, s, intSort.ptr))

  /** Creates a real number literal expression with sort [[realSort]].
   *  @param v value of the literal */
  def real(v: Int): Exp = new Exp(Native.mkInt(gcptr, v, realSort.ptr))

  /** Creates a real number literal expression from the fraction `n/d` with sort [[realSort]].
   *  @param n numerator of the fraction
   *  @param d denominator of the fraction */
  def real(n: Int, d: Int): Exp = new Exp(Native.mkReal(gcptr, n, d))

  /** Creates a real number literal expression with sort [[realSort]].
   *  @param v value of the literal */
  def real(v: Long): Exp = new Exp(Native.mkInt64(gcptr, v, realSort.ptr))

  /** Creates a real number literal expression with sort [[realSort]].
   *  @param s decimal string representing the value of the literal */
  def real(s: String): Exp = new Exp(Native.mkNumeral(gcptr, s, realSort.ptr))

  /** Creates a bit-vector literal expression with sort `bvSort(size)`
   *  @param v value of the literal */
  def bv(v: Int, size: Int): Exp = new Exp(Native.mkInt(gcptr, v, Native.mkBvSort(gcptr, size)))

  /** Creates a literal expression with value `v` and sort `s`.
   *  @param v value of the literal
   *  @param s must be [[intSort]] or [[realSort]]. */
  def literal(v: Int, s: Sort): Exp = s match {
    case _: IntSort | _: RealSort => new Exp(Native.mkInt(gcptr, v, s.ptr))
    case _ => throw new Z3Exception(s"Literal integer $v cannot have sort $s")
  }

  /** Creates a constant named `s` with sort [[boolSort]] */
  def boolConst(s: String): Exp = new Exp(Native.mkConst(gcptr, Native.mkStringSymbol(gcptr, s), Native.mkBoolSort(gcptr)))

  /** Creates a constant named `s` with sort [[intSort]] */
  def intConst(s: String): Exp = new Exp(Native.mkConst(gcptr, Native.mkStringSymbol(gcptr, s), Native.mkIntSort(gcptr)))

  /** Creates a constant named `s` with sort [[realSort]] */
  def realConst(s: String): Exp = new Exp(Native.mkConst(gcptr, Native.mkStringSymbol(gcptr, s), Native.mkRealSort(gcptr)))

  /** Creates a constant named `s` with sort `sort`. */
  def const(s: String, sort: Sort): Exp = new Exp(Native.mkConst(gcptr, Native.mkStringSymbol(gcptr, s), sort.ptr))

  /** Creates a function declaration.
   *  @param name name of the function declaration
   *  @param domain sorts of the function declaration's parameters
   *  @param range return type of the function declaration */
  def funcDecl(name: String, domain: Array[Sort], range: Sort): FuncDecl = new FuncDecl(
    Native.mkFuncDecl(gcptr, Native.mkStringSymbol(gcptr, name), domain.length, domain.map(_.ptr), range.ptr)
  )

  /** Creates a function application expression representing function `f` applied to `args`. */
  def app(f: FuncDecl, args: Exp*): Exp = new Exp(Native.mkApp(gcptr, f.ptr, args.length, args.map(_.ptr).toArray))

  /** Creates an expression equivalent to function `f` applied to `args` by substituting `args`
   *  into the definition of `f`. */
  def app(f: DefinedFunc, args: Exp*): Exp = {
    // TODO: investigate Native.substituteVars
    if (f.params.length != args.length)
      throw new Exception("Invalid function application: incorrect number of args")
    val varsToReplace = f.params.map{ case (param, sort) => Native.mkConst(gcptr, Native.mkStringSymbol(gcptr, param), sort.ptr) }
    val ePtr = Native.substitute(gcptr, f.body.ptr, args.length, varsToReplace.toArray, args.map(_.ptr).toArray)
    new Exp(ePtr)
  }

  //---------- Propositional Logic and Equality --------------------------------

  /** Creates an equality expression with sort [[boolSort]]. The sorts of `e1` and `e2` should be the same. */
  def equ(e1: Exp, e2: Exp): Exp = new Exp(Native.mkEq(gcptr, e1.ptr, e2.ptr))

  /** Creates an expression that means no two expressions in `es` are equal to each other. */
  def distinct(es: Exp*): Exp = new Exp(Native.mkDistinct(gcptr, es.length, es.map(_.ptr).toArray))

  /** Creates a logical NOT expression. */
  def not(e: Exp): Exp = new Exp(Native.mkNot(gcptr, e.ptr))

  /** Creates an `if-then-else` expression. */
  def ite(condE: Exp, thenE: Exp, elseE: Exp): Exp = new Exp(Native.mkIte(gcptr, condE.ptr, thenE.ptr, elseE.ptr))

  /** Creates a logical implication expression. Expressions `e1` and `e2` should be boolean-sorted.
   *  @return expression that means `e1` ''logically implies'' `e2`. */
  def implies(e1: Exp, e2: Exp): Exp = new Exp(Native.mkImplies(gcptr, e1.ptr, e2.ptr))

  /** Creates a logical AND expression. */
  def and(es: Exp*): Exp = new Exp(Native.mkAnd(gcptr, es.length, es.map(_.ptr).toArray))

  /** Creates a logical OR expression. */
  def or(es: Exp*): Exp = new Exp(Native.mkOr(gcptr, es.length, es.map(_.ptr).toArray))

  //---------- Integers and Reals ----------------------------------------------

  /** Creates an addition expression. */
  def add(es: Exp*): Exp = new Exp(Native.mkAdd(gcptr, es.length, es.map(_.ptr).toArray))

  /** Creates a multiplication expression. */
  def mul(es: Exp*): Exp = new Exp(Native.mkMul(gcptr, es.length, es.map(_.ptr).toArray))

  /** Creates a subtraction expression. */
  def sub(e1: Exp, e2: Exp): Exp = new Exp(Native.mkSub(gcptr, 2, Array(e1.ptr, e2.ptr)))

  /** Creates a unary minus expression. */
  def neg(e: Exp): Exp = new Exp(Native.mkUnaryMinus(gcptr, e.ptr))

  /** Creates a division expression. */
  def div(e1: Exp, e2: Exp): Exp = new Exp(Native.mkDiv(gcptr, e1.ptr, e2.ptr))

  /** Modulo operation for integers. Result is always positive. Satisfies the property
   *  `mod(x, y) = mod(x+y, y)` for all `x` and nonzero `y`. */
  def mod(e1: Exp, e2: Exp): Exp = new Exp(Native.mkMod(gcptr, e1.ptr, e2.ptr))

  /** Remainder operator for integers. Same as `mod` except the sign of the
   *  result matches the sign of `e2`. */
  def rem(e1: Exp, e2: Exp): Exp = new Exp(Native.mkRem(gcptr, e1.ptr, e2.ptr))

  /** Creates a power expression. */
  def power(e1: Exp, e2: Exp): Exp = new Exp(Native.mkPower(gcptr, e1.ptr, e2.ptr))

  /** Creates a less-than expression. */
  def lt(e1: Exp, e2: Exp): Exp = new Exp(Native.mkLt(gcptr, e1.ptr, e2.ptr))

  /** Creates a less-than-or-equal expression. */
  def le(e1: Exp, e2: Exp): Exp = new Exp(Native.mkLe(gcptr, e1.ptr, e2.ptr))

  /** Creates a greater-than expression. */
  def gt(e1: Exp, e2: Exp): Exp = new Exp(Native.mkGt(gcptr, e1.ptr, e2.ptr))

  /** Creates a greater-than-or-equal expression. */
  def ge(e1: Exp, e2: Exp): Exp = new Exp(Native.mkGe(gcptr, e1.ptr, e2.ptr))

  /** Creates division predicate. The expressions `t1` and `t2` must have sort `intSort`.
   *  The predicate is true when `t1` divides `t2`. For the predicate to be part of linear
   *  integer arithmetic, the first argument `t1` must be a non-zero integer.  */
  def divides(e1: Exp, e2: Exp): Exp = new Exp(Native.mkDivides(gcptr, e1.ptr, e2.ptr))

  /** Coerces an `intSort` to a `realSort`. */
  def int2real(e: Exp): Exp = new Exp(Native.mkInt2real(gcptr, e.ptr))

  /** Coerces a `realSort` to an `intSort`. */
  def real2int(e: Exp): Exp = new Exp(Native.mkReal2int(gcptr, e.ptr))

  /** Predicate that checks if real number `e` is an integer. */
  def isInt(e: Exp): Exp = new Exp(Native.mkIsInt(gcptr, e.ptr))

  //---------- Bit-vectors -----------------------------------------------------

  /** Bitwise NOT. */
  def bvnot(e: Exp): Exp = new Exp(Native.mkBvnot(gcptr, e.ptr))

  /** Bitwise AND. */
  def bvand(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvand(gcptr, e1.ptr, e2.ptr))

  /** Bitwise OR. */
  def bvor(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvor(gcptr, e1.ptr, e2.ptr))

  /** Two's complement unary minus. */
  def bvneg(e: Exp): Exp = new Exp(Native.mkBvneg(gcptr, e.ptr))

  /** Standard two's complement addition of bit-vectors. */
  def bvadd(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvadd(gcptr, e1.ptr, e2.ptr))

  def bvsub(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvsub(gcptr, e1.ptr, e2.ptr))

  def bvmul(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvmul(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned less than. */
  def bvult(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvult(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement less than. */
  def bvslt(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvslt(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned less than or equal to. */
  def bvule(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvule(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement less than or equal to.  */
  def bvsle(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvsle(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned greater than. */
  def bvugt(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvugt(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement greater than. */
  def bvsgt(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvsgt(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned greater than or equal to. */
  def bvuge(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvuge(gcptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement greater than or equal to. */
  def bvsge(e1: Exp, e2: Exp): Exp = new Exp(Native.mkBvsge(gcptr, e1.ptr, e2.ptr))

  //----------------------------------------------

  /** Creates a constant array. The resulting array has the property that a `select`
   *  on an arbitrary index produces the value `v`.
   *  @param domain the domain sort of the returned array
   *  @param v the constant value the returned array is filled with */
  def constArray(domain: Sort, v: Exp): Exp = new Exp(Native.mkConstArray(gcptr, domain.ptr, v.ptr))

  /** Applies the given `store` operations in succession to `constArray(domain, default)`.
   *  Note that later store operations overwrite previous stores for the same index.
   *  @see [[z3.constArray]], [[z3.store]], [[z3.buildIntArray]] */
  def buildArray(domain: Sort, default: Exp, stores: (Exp, Exp)*): Exp = {
    var aPtr = Native.mkConstArray(gcptr, domain.ptr, default.ptr)
    for ((d, r) <- stores) aPtr = Native.mkStore(gcptr, aPtr, d.ptr, r.ptr)
    new Exp(aPtr)
  }

  /** Applies the given `store` operations in succession to `constArray(intSort, int(default))`.
   *  @see [[z3.store]], [[z3.buildArray]]
   *  @example {{{buildIntArray(0, 1 -> 1, 2 -> 4, 3 -> 9, 4 -> 16)}}} */
  @inline final def buildIntArray(default: Int, stores: (Int, Int)*): Exp =
    buildArray(intSort, int(default), stores.map(p => int(p._1) -> int(p._2)):_*)

  /** Creates an "array read" expression. A `select` expression returns the value
   *  of the array `a` at index `i`. */
  def select(a: Exp, i: Exp): Exp = new Exp(Native.mkSelect(gcptr, a.ptr, i.ptr))

  /** Creates an "array write" expression. A `store` expression returns an array that is
   *  equivalent to `a` except that index `i` maps to value `v`. */
  def store(a: Exp, i: Exp, v: Exp): Exp = new Exp(Native.mkStore(gcptr, a.ptr, i.ptr, v.ptr))

  /** Returns an expression representing the array `a` with all of its values transformed by function `f`. */
  def map(a: Exp, f: FuncDecl): Exp = new Exp(Native.mkMap(gcptr, f.ptr, 1, Array(a.ptr)))

  /** A `map` that combines multiple arrays into one. The i-th array should have sort `[D -> R,,i,,]`.
   *  The function `f` should have sort `(R,,1,, ... R,,n,,) -> R`. The resulting array will have sort
   *  `[D -> R]`.*/
  def mapZipped(f: FuncDecl, arrays: Exp*): Exp = new Exp(Native.mkMap(gcptr, f.ptr, arrays.length, arrays.map(_.ptr).toArray))

  /** Creates a `forall` quantifier expression. The expression `boundVar` must be a constant that signifies the
   *  bound variable in `body` and `pattern`. */
  def forall(boundVar: Exp, body: Exp, pattern: Exp = null): Exp = if (pattern == null) {
    new Exp(Native.mkForallConst(gcptr, 0, 1, Array(boundVar.ptr), 0, Array.empty, body.ptr))
  } else {
    val pattern1 = Native.substitute(gcptr, pattern.ptr, 1, Array(boundVar.ptr), Array(Native.mkBound(gcptr, 0, boundVar.sort.ptr)))
    val pattern2 = Native.mkPattern(gcptr, 1, Array(pattern1))
    new Exp(Native.mkForallConst(gcptr, 0, 1, Array(boundVar.ptr), 1, Array(pattern2), body.ptr))
  }

  /** Creates an `exists` quantifier expression. The expression `boundVar` must be a constant that signifies the
   *  bound variable in `body` and `pattern`. */
  def exists(boundVar: Exp, body: Exp, pattern: Exp = null): Exp = if (pattern == null) {
    new Exp(Native.mkExistsConst(gcptr, 0, 1, Array(boundVar.ptr), 0, Array.empty, body.ptr))
  } else {
    val pattern1 = Native.substitute(gcptr, pattern.ptr, 1, Array(boundVar.ptr), Array(Native.mkBound(gcptr, 0, boundVar.sort.ptr)))
    val pattern2 = Native.mkPattern(gcptr, 1, Array(pattern1))
    new Exp(Native.mkExistsConst(gcptr, 0, 1, Array(boundVar.ptr), 1, Array(pattern2), body.ptr))
  }

  /** Alias for `checkSat(assertions.toArray)` */
  @inline final def checkSat(assertions: Exp*): Status = checkSat(assertions.toArray)

  /** Checks satisfiability of `assertions`.
   * @return a status indicating that the assertions are satisfiable, unsatisfiable, or that
   *         the solver failed to decide satisfiability. */
  def checkSat(assertions: Array[Exp]): Status = {
    for (assertion <- assertions)
      Native.solverAssert(gcptr, gsptr, assertion.ptr)

    val r: Z3_lbool = Z3_lbool.fromInt(Native.solverCheck(gcptr, gsptr))

    val ret = r match {
      case Z3_lbool.Z3_L_FALSE => UNSAT
      case Z3_lbool.Z3_L_UNDEF => UNKNOWN
      case Z3_lbool.Z3_L_TRUE => SAT(Model.create(Native.solverGetModel(gcptr, gsptr)))
    }

    Native.solverReset(gcptr, gsptr)

    ret
  }
}