package z3

import com.microsoft.z3.Native

/** A set of interpretations (assignments) of uninterpreted sorts, constants, and functions. */
case class Model(sorts: Map[String, List[Exp]], constants: Map[String, Exp], functions: Map[String, FuncInterp]) {
  override def toString: String = {
    sorts.map { case (k, v) => s"$k := { ${v.mkString(", ")} }\n" }.mkString +
    constants.map { case (k, v) => s"$k := $v\n" }.mkString +
    functions.map { case (k, v) => s"$k := $v" }.mkString("\n")
  }
}

object Model {
  private[z3] def create(job: Job, ptr: Long): Model = {
    Native.modelIncRef(job.cptr, ptr)

    val sorts: Map[String, List[Exp]] = (0 until Native.modelGetNumSorts(job.cptr, ptr)).map { i =>
      val sortPtr = Native.modelGetSort(job.cptr, ptr, i)
      val sortName = Native.getSymbolString(job.cptr, Native.getSortName(job.cptr, sortPtr))

      val universePtr = Native.modelGetSortUniverse(job.cptr, ptr, sortPtr)
      Native.astVectorIncRef(job.cptr, universePtr)
      val universe = (0 until Native.astVectorSize(job.cptr, universePtr)).map { i =>
        new Exp(job, Native.astVectorGet(job.cptr, universePtr, i))
      }.toList
      Native.astVectorDecRef(job.cptr, universePtr)

      sortName -> universe
    }.toMap

    val constants: Map[String, Exp] = (0 until Native.modelGetNumConsts(job.cptr, ptr)).map { i =>
      val declPtr = Native.modelGetConstDecl(job.cptr, ptr, i)
      val name = Native.getSymbolString(job.cptr, Native.getDeclName(job.cptr, declPtr))
      val interp = new Exp(job, Native.modelGetConstInterp(job.cptr, ptr, declPtr))
      name -> interp
    }.toMap

    val functions: Map[String, FuncInterp] = (0 until Native.modelGetNumFuncs(job.cptr, ptr)).map { i =>
      val declPtr = Native.modelGetFuncDecl(job.cptr, ptr, i)
      val name = Native.getSymbolString(job.cptr, Native.getDeclName(job.cptr, declPtr))
      val interp = FuncInterp.create(job, Native.modelGetFuncInterp(job.cptr, ptr, declPtr))
      name -> interp
    }.toMap

    Native.modelDecRef(job.cptr, ptr)
    Model(sorts, constants, functions)
  }
}

/** A finite map and an ''else'' value. Each entry in the finite map represents
 *  the value of a function given a set of arguments. */
case class FuncInterp(entries: List[Entry], elseValue: Exp) {
  override def toString: String = s"[\n  ${entries.map(_.toString + "\n  ").mkString}else |-> $elseValue\n]"
}

object FuncInterp {
  private[z3] def create(job: Job, ptr: Long): FuncInterp = {
    Native.funcInterpIncRef(job.cptr, ptr)

    val entries = (0 until Native.funcInterpGetNumEntries(job.cptr, ptr)).map { i =>
      Entry.create(job, Native.funcInterpGetEntry(job.cptr, ptr, i))
    }.toList
    val elseValue: Exp = new Exp(job, Native.funcInterpGetElse(job.cptr, ptr))

    Native.funcInterpDecRef(job.cptr, ptr)
    FuncInterp(entries, elseValue)
  }
}

/** An element in the finite map of a [[FuncInterp]]. */
case class Entry(args: List[Exp], value: Exp) {
  override def toString: String = s"(${args.mkString(", ")}) |-> $value"
}

object Entry {
  private[z3] def create(job: Job, ptr: Long): Entry = {
    Native.funcEntryIncRef(job.cptr, ptr)

    val args: List[Exp] = (0 until Native.funcEntryGetNumArgs(job.cptr, ptr)).map { i =>
      new Exp(job, Native.funcEntryGetArg(job.cptr, ptr, i))
    }.toList
    val value = new Exp(job, Native.funcEntryGetValue(job.cptr, ptr))

    Native.funcEntryDecRef(job.cptr, ptr)
    Entry(args, value)
  }
}