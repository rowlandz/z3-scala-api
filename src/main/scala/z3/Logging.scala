package z3

import com.microsoft.z3.Native

/** Start or stop logging of Z3 API calls to a file. */
object Logging {
  private var _started: Boolean = false

  /** Starts logging interaction with Z3 to `filename`.
   *  @return true if successfully started the log */
  def start(filename: String): Boolean = {
    val result = Native.openLog(filename)
    result == 1
  }

  /** Stops interaction logging. */
  def stop(): Unit = Native.closeLog()
}