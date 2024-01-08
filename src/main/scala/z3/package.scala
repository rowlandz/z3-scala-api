import com.microsoft.z3.{Native, Z3Exception}
import com.microsoft.z3.enumerations.Z3_lbool

/** A Scala interface to Z3. The most comfortable way to use Z3 in a single-threaded fashion is
 *  to make the main class extend [[Job]].
 *  {{{
 *    import z3._
 *
 *    class Main extends Job("/full/path/to/libz3.so") {
 *      def main(args: Array[String]): Unit = {
 *        println(checkSat(and(boolConst("p"), not(boolConst("p")))))
 *        close()
 *      }
 *    }
 *  }}}
 *
 *  For multi-threaded programs, each thread should have its own `Job` instance. The concurrency
 *  method used (e.g., `Thread`, `Future`) is up to the user.
 *  {{{
 *    import z3._
 *    import scala.concurrent.{Await, ExecutionContext, Future}
 *    import scala.concurrent.duration._

 *    object Main extends App {
 *      System.load("/full/path/to/libz3.so")
 *      val f1 = Future { new FactorJob(51).close() }(ExecutionContext.global)
 *      val f2 = Future { new FactorJob(21).close() }(ExecutionContext.global)
 *      Await.result(f1, 2.seconds)
 *      Await.result(f2, 2.seconds)
 *    }
 *
 *    class FactorJob(n: Int) extends Job {
 *      val x = intConst("x")
 *      val y = intConst("y")
 *      println(checkSat(
 *        ge(x, int(2)),
 *        ge(y, int(2)),
 *        equ(int(n), mul(x, y))
 *      ))
 *    }
 *  }}}
 */
package object z3 {
  /** Default configuration parameters for Z3.
   *  @see [[Parameters]] */
  val newParams: Parameters = new Parameters(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
}