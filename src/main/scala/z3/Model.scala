package z3

import com.microsoft.z3.Native
import z3.GlobalState.{gcptr => gcptr}

/** A set of interpretations (assignments) of uninterpreted sorts, constants, and functions. */
case class Model(sorts: Map[String, List[Exp]], constants: Map[String, Exp], functions: Map[String, FuncInterp]) {
  override def toString: String = {
    sorts.map { case (k, v) => s"$k := { ${v.mkString(", ")} }\n" }.mkString +
    constants.map { case (k, v) => s"$k := $v\n" }.mkString +
    functions.map { case (k, v) => s"$k := $v" }.mkString("\n")
  }
}

object Model {
  private[z3] def create(ptr: Long): Model = {
    Native.modelIncRef(gcptr, ptr)

    val sorts: Map[String, List[Exp]] = (0 until Native.modelGetNumSorts(gcptr, ptr)).map { i =>
      val sortPtr = Native.modelGetSort(gcptr, ptr, i)
      val sortName = Native.getSymbolString(gcptr, Native.getSortName(gcptr, sortPtr))

      val universePtr = Native.modelGetSortUniverse(gcptr, ptr, sortPtr)
      Native.astVectorIncRef(gcptr, universePtr)
      val universe = (0 until Native.astVectorSize(gcptr, universePtr)).map { i =>
        new Exp(Native.astVectorGet(gcptr, universePtr, i))
      }.toList
      Native.astVectorDecRef(gcptr, universePtr)

      sortName -> universe
    }.toMap

    val constants: Map[String, Exp] = (0 until Native.modelGetNumConsts(gcptr, ptr)).map { i =>
      val declPtr = Native.modelGetConstDecl(gcptr, ptr, i)
      val name = Native.getSymbolString(gcptr, Native.getDeclName(gcptr, declPtr))
      val interp = new Exp(Native.modelGetConstInterp(gcptr, ptr, declPtr))
      name -> interp
    }.toMap

    val functions: Map[String, FuncInterp] = (0 until Native.modelGetNumFuncs(gcptr, ptr)).map { i =>
      val declPtr = Native.modelGetFuncDecl(gcptr, ptr, i)
      val name = Native.getSymbolString(gcptr, Native.getDeclName(gcptr, declPtr))
      val interp = FuncInterp.create(Native.modelGetFuncInterp(gcptr, ptr, declPtr))
      name -> interp
    }.toMap

    Native.modelDecRef(gcptr, ptr)
    Model(sorts, constants, functions)
  }
}

/** A finite map and an ''else'' value. Each entry in the finite map represents
 *  the value of a function given a set of arguments. */
case class FuncInterp(entries: List[Entry], elseValue: Exp) {
  override def toString: String = s"[\n  ${entries.map(_.toString + "\n  ").mkString}else |-> $elseValue\n]"
}

object FuncInterp {
  private[z3] def create(ptr: Long): FuncInterp = {
    Native.funcInterpIncRef(gcptr, ptr)

    val entries = (0 until Native.funcInterpGetNumEntries(gcptr, ptr)).map { i =>
      Entry.create(Native.funcInterpGetEntry(gcptr, ptr, i))
    }.toList
    val elseValue: Exp = new Exp(Native.funcInterpGetElse(gcptr, ptr))

    Native.funcInterpDecRef(gcptr, ptr)
    FuncInterp(entries, elseValue)
  }
}

/** An element in the finite map of a [[FuncInterp]]. */
case class Entry(args: List[Exp], value: Exp) {
  override def toString: String = s"(${args.mkString(", ")}) |-> $value"
}

object Entry {
  private[z3] def create(ptr: Long): Entry = {
    Native.funcEntryIncRef(gcptr, ptr)

    val args: List[Exp] = (0 until Native.funcEntryGetNumArgs(gcptr, ptr)).map { i =>
      new Exp(Native.funcEntryGetArg(gcptr, ptr, i))
    }.toList
    val value = new Exp(Native.funcEntryGetValue(gcptr, ptr))

    Native.funcEntryDecRef(gcptr, ptr)
    Entry(args, value)
  }
}