package z3

import com.microsoft.z3.{Native, Z3Exception}
import com.microsoft.z3.enumerations.Z3_lbool

/** A Job must be created to use Z3. You can instantiate this class directly, but it
 *  is recommended to `extend` it so that its numerous members can be accessed without
 *  dot qualification. Jobs should be `close`d when they are no longer needed to avoid
 *  memory leaks.
 *
 *  Internally, a job manages a Z3 context and solver.
 *
 *  @constructor The creation of a new Job does five things:
 *               - Loads the `libz3.so` library (if its path is specified)
 *               - Sets Z3 global parameters
 *               - Processes job configuration parameters
 *               - Initializes and configures a Z3 context
 *               - Initializes and configures a Z3 solver
 *  @param libz3_path full path to the `libz3.so` library. If left blank, it is assumed that
 *                    the library is already loaded.
 *  @param params configuration options
 *  @see [[z3]], [[Parameters]]
 */
class Job(libz3_path: String = "", params: Parameters = newParams) {

  // Z3 does not provide a way to retrieve certain information about datatypes
  // after creation. So this Job needs to manually cache it. These Maps are
  // updated by the `datatypeSort` and `structSort` functions and accessed
  // by methods in the DatatypeSort class (and child classes).
  private[z3] var datatypeConstructorNames: Map[String, Array[String]] = Map.empty
  private[z3] var datatypeRecognizerNames: Map[String, Array[String]] = Map.empty
  private[z3] var datatypeFields: Map[String, Array[Array[Field]]] = Map.empty

  // If `false`, then `datatypeSort` and `structSort` will fail if
  // the created datatype would have the same name as an existing one.
  private val allowDatatypeOverwrite: Boolean = params.jobBool.getOrElse("allow_datatype_overwrite", false)

  // Load the Z3 library if necessary
  if (libz3_path.nonEmpty) System.load(libz3_path)

  // Set global parameters
  for ((k, v) <- params.global) Native.globalParamSet(k, v)

  // Create the context
  private[z3] val cptr: Long = Job.synchronized {
    if (params.context.nonEmpty) {
      val cfg: Long = Native.mkConfig()
      for ((key, value) <- params.context) Native.setParamValue(cfg, key, value)
      val cptr = Native.mkContext(cfg)
      Native.delConfig(cfg)
      cptr
    } else {
      Native.mkContext(0)
    }
  }

  // Create the solver
  private val sptr: Long = Native.mkSolver(cptr)
  Native.solverIncRef(cptr, sptr)
  if (params.solverInt.nonEmpty || params.solverString.nonEmpty || params.solverBool.nonEmpty) {
    val paramsPtr = Native.mkParams(cptr)
    Native.paramsIncRef(cptr, paramsPtr)
    for ((k, v) <- params.solverInt) Native.paramsSetUint(cptr, paramsPtr, Native.mkStringSymbol(cptr, k), v)
    for ((k, v) <- params.solverString) Native.paramsSetSymbol(cptr, paramsPtr, Native.mkStringSymbol(cptr, k), Native.mkStringSymbol(cptr, v))
    for ((k, v) <- params.solverBool) Native.paramsSetBool(cptr, paramsPtr, Native.mkStringSymbol(cptr, k), v)
    Native.solverSetParams(cptr, sptr, paramsPtr)
    Native.paramsDecRef(cptr, paramsPtr)
  }

  /** Relinquish resources used by this job. This method should be called when this job is no longer needed.
   *  Calling this method will render this job and all Z3 objects created with this job unusable. */
  def close(): Unit = Job.synchronized {
    if (cptr != 0) {
      if (sptr != 0) {
        Native.solverDecRef(cptr, sptr)
      }
      Native.delContext(cptr)
    }

    datatypeConstructorNames = Map.empty
    datatypeRecognizerNames = Map.empty
    datatypeFields = Map.empty
  }

  /** The sort that contains the booleans. */
  lazy val boolSort: Sort = new BoolSort(this, Native.mkBoolSort(cptr))

  /** The sort that contains the integers. */
  lazy val intSort: Sort = new IntSort(this, Native.mkIntSort(cptr))

  /** The sort that contains the real numbers. */
  lazy val realSort: Sort = new RealSort(this, Native.mkRealSort(cptr))

  /** The sort that contains unsigned or two's complement integers of `size` bits. */
  def bvSort(size: Int): BitVectorSort = new BitVectorSort(this, Native.mkBvSort(cptr, size))

  /** Creates a new non-recursive algebraic datatype. */
  def datatypeSort(name: String, constructors: Constructor*): DatatypeSort = {
    if (!allowDatatypeOverwrite && datatypeConstructorNames.contains(name))
      throw new Z3Exception(s"Datatype named $name already exists.")

    datatypeConstructorNames += (name -> constructors.map(_.constructorName).toArray)
    datatypeRecognizerNames += (name -> constructors.map(_.recognizerName).toArray)
    datatypeFields += (name -> constructors.map(_.fields).toArray)

    new DatatypeSort(this,
      Native.mkDatatype(cptr,
        Native.mkStringSymbol(cptr, name),
        constructors.length,
        constructors.map(_.ptr).toArray
      )
    )
  }

  /** Creates a constructor for a [[datatypeSort]]. */
  def constructor(name: String, recognizer: String, fields: Field*): Constructor = {
    val fieldNamePtrs = fields.map(f => Native.mkStringSymbol(cptr, f.name)).toArray
    val fieldSortPtrs = fields.map(_.sort.ptr).toArray

    new Constructor(
      Native.mkConstructor(cptr,
        Native.mkStringSymbol(cptr, name),
        Native.mkStringSymbol(cptr, recognizer),
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
    if (!allowDatatypeOverwrite && datatypeConstructorNames.contains(name))
      throw new Z3Exception(s"Datatype named $name already exists.")

    datatypeConstructorNames += (name -> Array(constructor))
    datatypeRecognizerNames += (name -> Array(recognizer))
    datatypeFields += (name -> Array(fields.toArray))

    new StructSort(this,
      Native.mkDatatype(cptr,
        Native.mkStringSymbol(cptr, name),
        1,
        Array(this.constructor(constructor, recognizer, fields: _*).ptr)
      )
    )
  }

  /** The array sort containing values of sort `range` indexed by `domain`. */
  def arraySort(domain: Sort, range: Sort): ArraySort =
    new ArraySort(this, Native.mkArraySort(cptr, domain.ptr, range.ptr))

  def uninterpretedSort(name: String): UninterpretedSort =
    new UninterpretedSort(this, Native.mkUninterpretedSort(cptr, Native.mkStringSymbol(cptr, name)))

  /** Creates a new non-recursive generic data type. */
  @inline final def genericDatatype(name: String, tParams: List[String], constructors: GenericConstructor*): GenericDatatype =
    new GenericDatatype(this, name, tParams, constructors: _*)

  @inline final def genericConstructor(name: String, recognizer: String, fields: GenericField*): GenericConstructor =
    new GenericConstructor(this, name, recognizer, fields: _*)

  @inline final def genericField(name: String, sort: Sort): GenericField = GenericField(name, Left(sort))

  @inline final def genericField(name: String, tparam: String): GenericField = GenericField(name, Right(tparam))

  /** Creates a new function definition. When used in expressions, the body of a defined function is automatically
   *  substituted for the function call. */
  @inline final def definedFunc(name: String, params: List[(String, Sort)], range: Sort, body: Exp): DefinedFunc =
    new DefinedFunc(this, name, params, range, body)

  /** Creates a boolean literal expression with sort [[boolSort]].
   *  @param b value of the literal */
  def bool(b: Boolean): Exp = new Exp(this, if (b) Native.mkTrue(cptr) else Native.mkFalse(cptr))

  /** Creates an integer literal expression with sort [[intSort]].
   *  @param v value of the literal */
  def int(v: Int): Exp = new Exp(this, Native.mkInt(cptr, v, intSort.ptr))

  /** Creates an integer literal expression with sort [[intSort]].
   *  @param v value of the literal */
  def int(v: Long): Exp = new Exp(this, Native.mkInt64(cptr, v, intSort.ptr))

  /** Creates an integer literal expression with sort [[intSort]].
   *  @param s decimal string representing the value of the literal */
  def int(s: String): Exp = new Exp(this, Native.mkNumeral(cptr, s, intSort.ptr))

  /** Creates a real number literal expression with sort [[realSort]].
   *  @param v value of the literal */
  def real(v: Int): Exp = new Exp(this, Native.mkInt(cptr, v, realSort.ptr))

  /** Creates a real number literal expression from the fraction `n/d` with sort [[realSort]].
   *  @param n numerator of the fraction
   *  @param d denominator of the fraction */
  def real(n: Int, d: Int): Exp = new Exp(this, Native.mkReal(cptr, n, d))

  /** Creates a real number literal expression with sort [[realSort]].
   *  @param v value of the literal */
  def real(v: Long): Exp = new Exp(this, Native.mkInt64(cptr, v, realSort.ptr))

  /** Creates a real number literal expression with sort [[realSort]].
   *  @param s decimal string representing the value of the literal */
  def real(s: String): Exp = new Exp(this, Native.mkNumeral(cptr, s, realSort.ptr))

  /** Creates a bit-vector literal expression with sort `bvSort(size)`
   *  @param v value of the literal */
  def bv(v: Int, size: Int): Exp = new Exp(this, Native.mkInt(cptr, v, Native.mkBvSort(cptr, size)))

  /** Creates a literal expression with value `v` and sort `s`.
   *  @param v value of the literal
   *  @param s must be [[intSort]] or [[realSort]]. */
  def literal(v: Int, s: Sort): Exp = s match {
    case _: IntSort | _: RealSort => new Exp(this, Native.mkInt(cptr, v, s.ptr))
    case _ => throw new Z3Exception(s"Literal integer $v cannot have sort $s")
  }

  /** Creates a constant named `s` with sort [[boolSort]] */
  def boolConst(s: String): Exp = new Exp(this, Native.mkConst(cptr, Native.mkStringSymbol(cptr, s), Native.mkBoolSort(cptr)))

  /** Creates a constant named `s` with sort [[intSort]] */
  def intConst(s: String): Exp = new Exp(this, Native.mkConst(cptr, Native.mkStringSymbol(cptr, s), Native.mkIntSort(cptr)))

  /** Creates a constant named `s` with sort [[realSort]] */
  def realConst(s: String): Exp = new Exp(this, Native.mkConst(cptr, Native.mkStringSymbol(cptr, s), Native.mkRealSort(cptr)))

  /** Creates a constant named `s` with sort `sort`. */
  def const(s: String, sort: Sort): Exp = new Exp(this, Native.mkConst(cptr, Native.mkStringSymbol(cptr, s), sort.ptr))

  /** Creates a function declaration.
   *  @param name   name of the function declaration
   *  @param domain sorts of the function declaration's parameters
   *  @param range  return type of the function declaration */
  def funcDecl(name: String, domain: Array[Sort], range: Sort): FuncDecl = new FuncDecl(this,
    Native.mkFuncDecl(cptr, Native.mkStringSymbol(cptr, name), domain.length, domain.map(_.ptr), range.ptr)
  )

  /** Creates a function application expression representing function `f` applied to `args`. */
  def app(f: FuncDecl, args: Exp*): Exp = new Exp(this, Native.mkApp(cptr, f.ptr, args.length, args.map(_.ptr).toArray))

  /** Creates an expression equivalent to function `f` applied to `args` by substituting `args`
   *  into the definition of `f`. */
  def app(f: DefinedFunc, args: Exp*): Exp = {
    if (f.params.length != args.length)
      throw new Exception("Invalid function application: incorrect number of args")
    val varsToReplace = f.params.map { case (param, sort) => Native.mkConst(cptr, Native.mkStringSymbol(cptr, param), sort.ptr) }
    val ePtr = Native.substitute(cptr, f.body.ptr, args.length, varsToReplace.toArray, args.map(_.ptr).toArray)
    new Exp(this, ePtr)
  }

  //---------- Propositional Logic and Equality --------------------------------

  /** Creates an equality expression with sort [[boolSort]]. The sorts of `e1` and `e2` should be the same. */
  def equ(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkEq(cptr, e1.ptr, e2.ptr))

  /** Creates an expression that means no two expressions in `es` are equal to each other. */
  def distinct(es: Exp*): Exp = new Exp(this, Native.mkDistinct(cptr, es.length, es.map(_.ptr).toArray))

  /** Creates a logical NOT expression. */
  def not(e: Exp): Exp = new Exp(this, Native.mkNot(cptr, e.ptr))

  /** Creates an `if-then-else` expression. */
  def ite(condE: Exp, thenE: Exp, elseE: Exp): Exp = new Exp(this, Native.mkIte(cptr, condE.ptr, thenE.ptr, elseE.ptr))

  /** Creates a logical implication expression. Expressions `e1` and `e2` should be boolean-sorted.
   *  @return expression that means `e1` ''logically implies'' `e2`. */
  def implies(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkImplies(cptr, e1.ptr, e2.ptr))

  /** Creates a logical AND expression. */
  def and(es: Exp*): Exp = new Exp(this, Native.mkAnd(cptr, es.length, es.map(_.ptr).toArray))

  /** Creates a logical OR expression. */
  def or(es: Exp*): Exp = new Exp(this, Native.mkOr(cptr, es.length, es.map(_.ptr).toArray))

  //---------- Integers and Reals ----------------------------------------------

  /** Creates an addition expression. */
  def add(es: Exp*): Exp = new Exp(this, Native.mkAdd(cptr, es.length, es.map(_.ptr).toArray))

  /** Creates a multiplication expression. */
  def mul(es: Exp*): Exp = new Exp(this, Native.mkMul(cptr, es.length, es.map(_.ptr).toArray))

  /** Creates a subtraction expression. */
  def sub(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkSub(cptr, 2, Array(e1.ptr, e2.ptr)))

  /** Creates a unary minus expression. */
  def neg(e: Exp): Exp = new Exp(this, Native.mkUnaryMinus(cptr, e.ptr))

  /** Creates a division expression. */
  def div(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkDiv(cptr, e1.ptr, e2.ptr))

  /** Modulo operation for integers. Result is always positive. Satisfies the property
   *  `mod(x, y) = mod(x+y, y)` for all `x` and nonzero `y`. */
  def mod(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkMod(cptr, e1.ptr, e2.ptr))

  /** Remainder operator for integers. Same as `mod` except the sign of the
   *  result matches the sign of `e2`. */
  def rem(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkRem(cptr, e1.ptr, e2.ptr))

  /** Creates a power expression. */
  def power(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkPower(cptr, e1.ptr, e2.ptr))

  /** Creates a less-than expression. */
  def lt(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkLt(cptr, e1.ptr, e2.ptr))

  /** Creates a less-than-or-equal expression. */
  def le(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkLe(cptr, e1.ptr, e2.ptr))

  /** Creates a greater-than expression. */
  def gt(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkGt(cptr, e1.ptr, e2.ptr))

  /** Creates a greater-than-or-equal expression. */
  def ge(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkGe(cptr, e1.ptr, e2.ptr))

  /** Creates division predicate. The expressions `t1` and `t2` must have sort `intSort`.
   *  The predicate is true when `t1` divides `t2`. For the predicate to be part of linear
   *  integer arithmetic, the first argument `t1` must be a non-zero integer. */
  def divides(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkDivides(cptr, e1.ptr, e2.ptr))

  /** Coerces an `intSort` to a `realSort`. */
  def int2real(e: Exp): Exp = new Exp(this, Native.mkInt2real(cptr, e.ptr))

  /** Coerces a `realSort` to an `intSort`. */
  def real2int(e: Exp): Exp = new Exp(this, Native.mkReal2int(cptr, e.ptr))

  /** Predicate that checks if real number `e` is an integer. */
  def isInt(e: Exp): Exp = new Exp(this, Native.mkIsInt(cptr, e.ptr))

  //---------- Bit-vectors -----------------------------------------------------

  /** Bitwise NOT. */
  def bvnot(e: Exp): Exp = new Exp(this, Native.mkBvnot(cptr, e.ptr))

  /** Bitwise AND. */
  def bvand(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvand(cptr, e1.ptr, e2.ptr))

  /** Bitwise OR. */
  def bvor(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvor(cptr, e1.ptr, e2.ptr))

  /** Two's complement unary minus. */
  def bvneg(e: Exp): Exp = new Exp(this, Native.mkBvneg(cptr, e.ptr))

  /** Standard two's complement addition of bit-vectors. */
  def bvadd(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvadd(cptr, e1.ptr, e2.ptr))

  /** Standard two's complement subtraction of bit-vectors. */
  def bvsub(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvsub(cptr, e1.ptr, e2.ptr))

  /** Standard two's complement multiplication of bit-vectors. */
  def bvmul(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvmul(cptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned less than. */
  def bvult(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvult(cptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement less than. */
  def bvslt(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvslt(cptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned less than or equal to. */
  def bvule(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvule(cptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement less than or equal to. */
  def bvsle(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvsle(cptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned greater than. */
  def bvugt(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvugt(cptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement greater than. */
  def bvsgt(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvsgt(cptr, e1.ptr, e2.ptr))

  /** Bit-vector unsigned greater than or equal to. */
  def bvuge(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvuge(cptr, e1.ptr, e2.ptr))

  /** Bit-vector two's complement greater than or equal to. */
  def bvsge(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkBvsge(cptr, e1.ptr, e2.ptr))

  //---------- Floating-Point Arithmetic ---------------------------------------

  /** Single-precision IEEE-754 floating-point sort. */
  lazy val fp32sort: FPSort = new FPSort(this, Native.mkFpaSort32(cptr))

  /** Double-precision IEEE-754 floating-point sort. */
  lazy val fp64sort: FPSort = new FPSort(this, Native.mkFpaSort64(cptr))

  /** Creates a value of sort `fp32sort`. */
  def fp32(v: Float): Exp = new Exp(this, Native.mkFpaNumeralFloat(cptr, v, fp32sort.ptr))

  /** Creates a value of sort `fp64sort`. */
  def fp64(v: Double): Exp = new Exp(this, Native.mkFpaNumeralDouble(cptr, v, fp64sort.ptr))

  /** "Not a number" value of sort `fp32sort`. */
  lazy val nan32: Exp = new Exp(this, Native.mkFpaNan(cptr, fp32sort.ptr))

  /** "Not a number" value of sort `fp64sort`. */
  lazy val nan64: Exp = new Exp(this, Native.mkFpaNan(cptr, fp64sort.ptr))

  /** Positive infinity of sort `fp32sort`. */
  lazy val infinity32: Exp = new Exp(this, Native.mkFpaInf(cptr, fp32sort.ptr, false))

  /** Negative infinity of sort `fp32sort`. */
  lazy val negativeInfinity32: Exp = new Exp(this, Native.mkFpaInf(cptr, fp32sort.ptr, true))

  /** Floating-point negation. */
  def fpneg(e: Exp): Exp = new Exp(this, Native.mkFpaNeg(cptr, e.ptr))

  /** Addition of IEEE-754 floating-point numbers. */
  def fpadd(e1: Exp, e2: Exp)(implicit rm: RoundingMode = NearestTiesToEven): Exp =
    new Exp(this, Native.mkFpaAdd(cptr, rm.toNative(this), e1.ptr, e2.ptr))

  /** Subtraction of IEEE-754 floating-point numbers. */
  def fpsub(e1: Exp, e2: Exp)(implicit rm: RoundingMode = NearestTiesToEven): Exp =
    new Exp(this, Native.mkFpaSub(cptr, rm.toNative(this), e1.ptr, e2.ptr))

  /** Multiplication of IEEE-754 floating-point numbers. */
  def fpmul(e1: Exp, e2: Exp)(implicit rm: RoundingMode = NearestTiesToEven): Exp =
    new Exp(this, Native.mkFpaMul(cptr, rm.toNative(this), e1.ptr, e2.ptr))

  /** Floating-point less than. */
  def fplt(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkFpaLt(cptr, e1.ptr, e2.ptr))

  /** Floating-point less than or equal to. */
  def fple(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkFpaLeq(cptr, e1.ptr, e2.ptr))

  /** Floating-point greater than. */
  def fpgt(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkFpaGt(cptr, e1.ptr, e2.ptr))

  /** Floating-point greater than or equal to. */
  def fpge(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkFpaGeq(cptr, e1.ptr, e2.ptr))

  /** Floating-point equality. Floating-point equality differs from [[equ]] in two ways:
   *  - NaN is not equal to anything (including itself)
   *  - Positive zero is equal to negative zero */
  def fpeq(e1: Exp, e2: Exp): Exp = new Exp(this, Native.mkFpaEq(cptr, e1.ptr, e2.ptr))

  //----------------------------------------------------------------------------

  /** Creates a constant array. The resulting array has the property that a `select`
   *  on an arbitrary index produces the value `v`.
   *  @param domain the domain sort of the returned array
   *  @param v      the constant value the returned array is filled with */
  def constArray(domain: Sort, v: Exp): Exp = new Exp(this, Native.mkConstArray(cptr, domain.ptr, v.ptr))

  /** Applies the given `store` operations in succession to `constArray(domain, default)`.
   *  Note that later store operations overwrite previous stores for the same index.
   *  @see [[constArray]], [[store]], [[buildIntArray]] */
  def buildArray(domain: Sort, default: Exp, stores: (Exp, Exp)*): Exp = {
    var aPtr = Native.mkConstArray(cptr, domain.ptr, default.ptr)
    for ((d, r) <- stores) aPtr = Native.mkStore(cptr, aPtr, d.ptr, r.ptr)
    new Exp(this, aPtr)
  }

  /** Applies the given `store` operations in succession to `constArray(intSort, int(default))`.
   *  @see [[store]], [[buildArray]]
   *  @example {{{buildIntArray(0, 1 -> 1, 2 -> 4, 3 -> 9, 4 -> 16)}}} */
  @inline final def buildIntArray(default: Int, stores: (Int, Int)*): Exp =
    buildArray(intSort, int(default), stores.map(p => int(p._1) -> int(p._2)): _*)

  /** Creates an "array read" expression. A `select` expression returns the value
   *  of the array `a` at index `i`. */
  def select(a: Exp, i: Exp): Exp = new Exp(this, Native.mkSelect(cptr, a.ptr, i.ptr))

  /** Creates an "array write" expression. A `store` expression returns an array that is
   *  equivalent to `a` except that index `i` maps to value `v`. */
  def store(a: Exp, i: Exp, v: Exp): Exp = new Exp(this, Native.mkStore(cptr, a.ptr, i.ptr, v.ptr))

  /** Returns an expression representing the array `a` with all of its values transformed by function `f`. */
  def map(a: Exp, f: FuncDecl): Exp = new Exp(this, Native.mkMap(cptr, f.ptr, 1, Array(a.ptr)))

  /** A `map` that combines multiple arrays into one. The i-th array should have sort `[D -> R,,i,,]`.
   *  The function `f` should have sort `(R,,1,, ... R,,n,,) -> R`. The resulting array will have sort
   *  `[D -> R]`. */
  def mapZipped(f: FuncDecl, arrays: Exp*): Exp = new Exp(this, Native.mkMap(cptr, f.ptr, arrays.length, arrays.map(_.ptr).toArray))

  /** Creates a `forall` quantifier expression. The expression `boundVar` must be a constant that signifies the
   *  bound variable in `body` and `pattern`. */
  def forall(boundVar: Exp, body: Exp, pattern: Exp = null): Exp = if (pattern == null) {
    new Exp(this, Native.mkForallConst(cptr, 0, 1, Array(boundVar.ptr), 0, Array.empty, body.ptr))
  } else {
    val pattern1 = Native.substitute(cptr, pattern.ptr, 1, Array(boundVar.ptr), Array(Native.mkBound(cptr, 0, boundVar.sort.ptr)))
    val pattern2 = Native.mkPattern(cptr, 1, Array(pattern1))
    new Exp(this, Native.mkForallConst(cptr, 0, 1, Array(boundVar.ptr), 1, Array(pattern2), body.ptr))
  }

  /** Creates an `exists` quantifier expression. The expression `boundVar` must be a constant that signifies the
   *  bound variable in `body` and `pattern`. */
  def exists(boundVar: Exp, body: Exp, pattern: Exp = null): Exp = if (pattern == null) {
    new Exp(this, Native.mkExistsConst(cptr, 0, 1, Array(boundVar.ptr), 0, Array.empty, body.ptr))
  } else {
    val pattern1 = Native.substitute(cptr, pattern.ptr, 1, Array(boundVar.ptr), Array(Native.mkBound(cptr, 0, boundVar.sort.ptr)))
    val pattern2 = Native.mkPattern(cptr, 1, Array(pattern1))
    new Exp(this, Native.mkExistsConst(cptr, 0, 1, Array(boundVar.ptr), 1, Array(pattern2), body.ptr))
  }

  /** Alias for `checkSat(assertions.toArray)` */
  @inline final def checkSat(assertions: Exp*): Status = checkSat(assertions.toArray)

  /** Checks satisfiability of `assertions`.
   *  @return a status indicating that the assertions are satisfiable, unsatisfiable, or that
   *          the solver failed to decide satisfiability. */
  def checkSat(assertions: Array[Exp]): Status = {
    for (assertion <- assertions)
      Native.solverAssert(cptr, sptr, assertion.ptr)

    val r: Z3_lbool = Z3_lbool.fromInt(Native.solverCheck(cptr, sptr))

    val ret = r match {
      case Z3_lbool.Z3_L_FALSE => UNSAT
      case Z3_lbool.Z3_L_UNDEF => UNKNOWN
      case Z3_lbool.Z3_L_TRUE => SAT(Model.create(this, Native.solverGetModel(cptr, sptr)))
    }

    Native.solverReset(cptr, sptr)

    ret
  }
}

// Right now, this is just used to synchronize creation/deletion of Z3 contexts.
private[z3] object Job {}