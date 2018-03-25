package example

// PCG pseudo-random generator. See http://www.pcg-random.org/download.html
object PCGRandom {

  final case class InternalState(state: Long, inc: Long) // These numbers are 64-bit unsigned integers.

  def initialDefault: InternalState = InternalState(// If you *must* statically initialize it, here's one.
    0x853c49e6748fea9bL, 0xda3e39cb94b95bdbL
  )

  /** Generate a uniformly distributed 32-bit pseudo-random number.
    *
    * @param internalState The initial internal state of the generator.
    * @return A random number and the updated internal state.
    */
  def int32(internalState: InternalState): (InternalState, Int) = {
    val oldState = internalState.state
    val newState = oldState * 6364136223846793005L + (internalState.inc | 1L) // `inc` must be always odd, according to the documentation.
    val xorshifted: Int = (((oldState >> 18) ^ oldState) >> 27).toInt
    val rot: Int = (oldState >> 59).toInt
    val result: Int = (xorshifted >> rot) | (xorshifted << ((-rot) & 31))
    (InternalState(newState, internalState.inc), result)
  }
}
