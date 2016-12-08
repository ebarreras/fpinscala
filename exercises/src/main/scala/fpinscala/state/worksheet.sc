import fpinscala.state.{Machine, State}
import fpinscala.state.RNG._
import fpinscala.state._
import fpinscala.state.State._


val rng = Simple(12345L)

ints(5)(rng)
intsViaSequence(5)(rng)

List.fill(4)(List(Coin,Turn)).flatten

val m = Machine(true, 5, 10)

coin.andThen(turn)(m)

State.simulateMachine(List.fill(4)(List(Coin,Turn)).flatten).run(m)

