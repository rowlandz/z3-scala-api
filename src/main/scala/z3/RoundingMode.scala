package z3

import com.microsoft.z3.Native

/** Specifies the precise behavior of arithmetic operations for floating-point numbers. */
sealed trait RoundingMode {
  private[z3] def toNative(job: Job): Long
}

case object NearestTiesToEven extends RoundingMode {
  override private[z3] def toNative(job: Job): Long = Native.mkFpaRoundNearestTiesToEven(job.cptr)
}

case object NearestTiesToAway extends RoundingMode {
  override private[z3] def toNative(job: Job): Long = Native.mkFpaRoundNearestTiesToAway(job.cptr)
}

case object TowardPositive extends RoundingMode{
  override private[z3] def toNative(job: Job): Long = Native.mkFpaRoundTowardPositive(job.cptr)
}

case object TowardNegative extends RoundingMode {
  override private[z3] def toNative(job: Job): Long = Native.mkFpaRoundTowardNegative(job.cptr)
}

case object TowardZero extends RoundingMode {
  override private[z3] def toNative(job: Job): Long = Native.mkFpaRoundTowardZero(job.cptr)
}