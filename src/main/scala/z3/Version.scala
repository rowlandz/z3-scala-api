package z3

import com.microsoft.z3.Native

/** Version information about the Z3 build being used. */
object Version {

  lazy val (major: Int, minor: Int, build: Int, revision: Int) = {
    val major = new Native.IntPtr()
    val minor = new Native.IntPtr()
    val build = new Native.IntPtr()
    val revision = new Native.IntPtr()

    Native.getVersion(major, minor, build, revision)

    (major.value, minor.value, build.value, revision.value)
  }

  lazy val full: String = s"$major.$minor.$build.$revision"
}