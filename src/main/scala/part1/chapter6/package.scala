package part1

package object chapter6 {
  type Rand[+A] = RNG => (A, RNG)

  /* TODO EXERCISE 6.11
     Hard: To gain experience with the use of State, implement a finite state automaton
     that models a simple candy dispenser. The machine has two types of input: you can
     insert a coin, or you can turn the knob to dispense candy. It can be in one of two
     states: locked or unlocked. It also tracks how many candies are left and how many
     coins it contains. */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)
  /* The rules of the machine are as follows:
     - Inserting a coin into a locked machine will cause it to unlock if there’s any
       candy left.
     - Turning the knob on an unlocked machine will cause it to dispense candy and
       become locked.
     - Turning the knob on a locked machine or inserting a coin into an unlocked
       machine does nothing.
     - A machine that’s out of candy ignores all inputs. */

  def takeInput(input: Input, state: State[Machine, Int]): State[Machine, Int] = ???
}