package z3

import com.microsoft.z3.Native
import GlobalState.{gcptr => gcptr}

/** A set of interpretations (assignments) of constants and functions. */
class ModelZ3 private[z3](private val ptr: Long) {
  Native.modelIncRef(gcptr, ptr)

  /** Interpretations of the constants in this model */
  lazy val constants: Map[String, Exp] = (0 until Native.modelGetNumConsts(gcptr, ptr)).map { i =>
    val declPtr = Native.modelGetConstDecl(gcptr, ptr, i)
    val name = Native.getSymbolString(gcptr, Native.getDeclName(gcptr, declPtr))
    val interp = new Exp(Native.modelGetConstInterp(gcptr, ptr, declPtr))
    name -> interp
  }.toMap

  /** Interpretations of the function declarations in this model */
  lazy val functions: Map[String, FuncInterpZ3] = (0 until Native.modelGetNumFuncs(gcptr, ptr)).map { i =>
    val declPtr = Native.modelGetFuncDecl(gcptr, ptr, i)
    val name = Native.getSymbolString(gcptr, Native.getDeclName(gcptr, declPtr))
    val interp = new FuncInterpZ3(Native.modelGetFuncInterp(gcptr, ptr, declPtr))
    name -> interp
  }.toMap

  override def toString: String =
    constants.map{ case (k, v) => s"$k := $v" }.mkString("\n") +
      functions.map{ case (k, v) => s"$k := $v" }.mkString("\n")

  private[z3] def destroy(): Unit = Native.modelDecRef(gcptr, ptr)
}

object ModelZ3 {
  def unapply(m: ModelZ3): Option[(Map[String, Exp], Map[String, FuncInterpZ3])] = Some(m.constants -> m.functions)
}


/** A finite map and an ''else'' value. Each entry in the finite map represents
 *  the value of a function given a set of arguments. */
class FuncInterpZ3 private[z3](private val ptr: Long) {
  Native.funcInterpIncRef(gcptr, ptr)

  /** The entries in the finite map */
  lazy val entries: List[EntryZ3] = (0 until Native.funcInterpGetNumEntries(gcptr, ptr)).map { i =>
    new EntryZ3(Native.funcInterpGetEntry(gcptr, ptr, i))
  }.toList

  /** The ''else'' value */
  lazy val elseValue: Exp = new Exp(Native.funcInterpGetElse(gcptr, ptr))

  /** Returns the number of parameters this function takes. */
  lazy val arity: Int = Native.funcInterpGetArity(gcptr, ptr)

  override def toString: String = s"[\n  ${entries.mkString("\n  ")}\n  else |-> $elseValue\n]"

  private[z3] def destroy(): Unit = Native.funcInterpDecRef(gcptr, ptr)
}


/** An element in the finite map of a [[FuncInterpZ3]]. */
class EntryZ3 private[z3](private val ptr: Long) {
  Native.funcEntryIncRef(gcptr, ptr)

  lazy val args: List[Exp] = (0 until Native.funcEntryGetNumArgs(gcptr, ptr)).map { i =>
    new Exp(Native.funcEntryGetArg(gcptr, ptr, i))
  }.toList

  lazy val value: Exp = new Exp(Native.funcEntryGetValue(gcptr, ptr))

  override def toString: String = s"(${args.mkString(", ")}) |-> $value"

  private[z3] def destroy(): Unit = Native.funcEntryDecRef(gcptr, ptr)
}