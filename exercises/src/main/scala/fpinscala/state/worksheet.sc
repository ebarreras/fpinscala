import fpinscala.state.RNG._

val rng = Simple(12345L)

ints(5)(rng)
intsViaSequence(5)(rng)

