package z3

import com.microsoft.z3.Native

private[z3] object GlobalState {
  var gcptr: Long = 0       // global context pointer
  var gsptr: Long = 0       // global solver pointer

  // Z3 does not provide a way to retrieve certain information about datatypes
  // after creation. So Scala API needs to manually cache it. These Maps are
  // updated by the `z3.datatypeSort` and `z3.structSort` functions and accessed
  // by methods in the DatatypeSort class (and child classes).
  var datatypeConstructorNames: Map[String, Array[String]] = Map.empty
  var datatypeRecognizerNames: Map[String, Array[String]] = Map.empty
  var datatypeFields: Map[String, Array[Array[Field]]] = Map.empty

  // If `false`, then `z3.datatypeSort` and `z3.structSort` will fail if
  // the created datatype would have the same name as an existing one.
  var allowDatatypeOverwrite: Boolean = false


  def startZ3(libz3_path: String, params: Parameters): Unit = {
    if (gcptr != 0) throw new Exception("Z3 is already started")

    // Load the Z3 library
    System.load(libz3_path)

    // Create global context
    GlobalState.synchronized {
      if (params.global.nonEmpty) {
        val cfg: Long = Native.mkConfig()
        for ((key, value) <- params.global) Native.setParamValue(cfg, key, value)
        gcptr = Native.mkContext(cfg)
        Native.delConfig(cfg)
      } else {
        gcptr = Native.mkContext(0)
      }
    }

    // Create global solver
    gsptr = Native.mkSolver(gcptr)
    Native.solverIncRef(gcptr, gsptr)
    if (params.solverInt.nonEmpty || params.solverString.nonEmpty || params.solverBool.nonEmpty) {
      val paramsPtr = Native.mkParams(gcptr)
      Native.paramsIncRef(gcptr, paramsPtr)
      for ((k, v) <- params.solverInt) Native.paramsSetUint(gcptr, paramsPtr, Native.mkStringSymbol(gcptr, k), v)
      for ((k, v) <- params.solverString) Native.paramsSetSymbol(gcptr, paramsPtr, Native.mkStringSymbol(gcptr, k), Native.mkStringSymbol(gcptr, v))
      for ((k, v) <- params.solverBool) Native.paramsSetBool(gcptr, paramsPtr, Native.mkStringSymbol(gcptr, k), v)
      Native.solverSetParams(gcptr, gsptr, paramsPtr)
      Native.paramsDecRef(gcptr, paramsPtr)
    }

    // Process API parameters
    for ((k, v) <- params.apiBool) k match {
      case "allow_datatype_overwrite" => allowDatatypeOverwrite = true
      case _ => throw new Exception("Unrecognized API parameter: " + k)
    }
  }

  def stopZ3(): Unit = GlobalState.synchronized {
    if (gcptr != 0) {
      if (gsptr != 0) {
        Native.solverDecRef(gcptr, gsptr)
        gsptr = 0
      }
      Native.delContext(gcptr)
      gcptr = 0
    }

    datatypeConstructorNames = Map.empty
    datatypeRecognizerNames = Map.empty
    datatypeFields = Map.empty

    allowDatatypeOverwrite = false
  }
}