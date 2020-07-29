import scala.collection.mutable

object Main extends App {
  // transition tuple : *(State, Input, Output, Direction, NewState)*
  sealed trait Direction
  case object R extends Direction
  case object L extends Direction

  // tape alphabet
  sealed trait Symbol
  case object _0 extends Symbol
  case object _1 extends Symbol

  case class State(s: String)

  type CurrState = State
  type NextState = State

  type Input = Symbol
  type Output = Symbol

  type Transition = (CurrState, Input, Output, Direction, NextState)

  /*

  Definition 1. A Turing machine is a quadruple (Q ∪ {z}, Γ, δ, a) where
  • z is a distinguished state called a halting state
  • Γ is the tape alphabet
  • δ is a partial function from Q × Γ to Q ∪ {z} × Γ × {l, r} called the transition function
  • a ∈ Q is a distinguished state called the start state

  */

  object Tape {
    def mkArr(size: Int): Array[Symbol] = Array.fill[Symbol](size)(_0)

    // could be generalized i guess? issue is with mkArr, i want to all entries on tape
    // to be default symbol.
    def fitToSize(arr: Array[Symbol], size: Int): Array[Symbol] =
      if (size >= arr.length) {
        // use long to avoid overflow issues during calculations.
        // restricting newSize to Int.MaxValue later on and casting to int.
        var newSize: Long = arr.length * 2
        while (size > newSize)
          newSize = newSize * 2

        if (newSize > Int.MaxValue)
          newSize = Int.MaxValue

        val newArray = mkArr(newSize.toInt)
        java.lang.System.arraycopy(arr, 0, newArray, 0, arr.length)
        newArray
      } else {
        arr
      }
  }

  class Tape {
    import Tape._

    var pos = 0
    var arrPos = mkArr(256)
    var arrNeg = mkArr(256)

    private def arr: Array[Symbol] = if (pos >= 0) arrPos else arrNeg

    def curr: Symbol = arr.apply(pos)
    def setAndMove(s: Symbol, d: Direction): Unit = {
      arr.update(math.abs(pos), s)
      pos += (if (d ==  R) 1 else -1)
      if (pos >= 0)
        arrPos = fitToSize(arrPos, math.abs(pos))
      else
        arrNeg = fitToSize(arrNeg, math.abs(pos))
    }
  }

  case class TM(
    n: Int = 5,
    m: Int = 2,
    transitions: mutable.Map[(CurrState, Input), (Output, Direction, NextState)] = mutable.HashMap.empty,
    states: mutable.Set[State] = mutable.HashSet.empty,
    symbols: mutable.Set[Symbol] = mutable.HashSet.empty,
    tape: Tape = new Tape,
    var state: State = State("a"),
  ) {
    def withTransition(t: Transition): TM = {
      val newTransitions = transitions.clone()
      ts((t._1, t._2)) = (t._3, t._4, t._5)

      val newStates = states.clone()
      states.add(t._1)
      states.add(t._5)
      symbols.add(t._2)
      symbols.add(t._3)

      copy(
        transitions = ts,
        states = states.clone(),

      )
      TM(n, m, transitions.clone(), states.clone(), symbols.clone(), tape.clone(), )
    }

    def setTransitions(ts: Seq[Transition]): Unit = ts.foreach { t =>
      transitions((t._1, t._2)) = (t._3, t._4, t._5)
      states.add(t._1)
      states.add(t._5)
      symbols.add(t._2)
      symbols.add(t._3)
    }

    def nStateFull: Boolean = states.size == n
    def mSymbolFull: Boolean = symbols.size == m

    def zeroDextrous: Boolean = {
      val x = transitions.filter { case ((_, 0), (_, _, _)) => true; case _ => false }

      // the check is performed for M ∪ {(S, I, 1, r, z)} so if input (I) is 0 we need to take
      // it into account, otherwise not
      (x.size == (if (tape.curr == _0) n-1 else n)) &&
        x.forall{ case ((_, 0), (_, R, _)) => true; case _ => false }
    }

    def step(): Unit = {
      transitions.get((state, tape.curr)) match {
        case Some((out, dir, ns)) =>
          state = ns
          tape.setAndMove(out, dir)

        case None =>
          val zd = zeroDextrous
          // 4_1
          // n state full, m symbol full == all states and symbols have been GENERATED

          // 4_2
      }
    }

    // grow machines transitions and keep executing
    // TODO:
    //  - discard "irrelevant" machines as soon as it's obvious
    //  - return # steps done ?
    def execute(maxSteps: Int): Unit = {
      // execute until:::
      // M is known to be irrelevant, or
      // the bound on the number of execution steps is exceeded.

      // OH OH we need to generate more states:
      // an undefined combination of state S and input I is found.
    }
  }

  def generateHelp(m: TM): Iterable[TM] = ???

  // TODO:
  //  - m (tape alphabet size) is hardcoded to 2 atm
  //  - memory optimization (generate states once, various collections setc)
  //  - parallel generation of machines
  def generate(n: Int): Iterable[TM] = {
    val m = new TM
    m.setTransitions(Seq((State("a"), _0, _1, R, State("b"))))



    val step2_1: Seq[Transition] = step1 ++
    for {
      ns <- Seq(State("a"), State("b"))
      out <- Seq(_0, _1)
    } yield (State("b"), _0, out, L, ns)

    val step2_2: Seq[Transition] = step2_1 ++
    if (n >= 3) {
      for {
        d <- Seq(R, L)
        out <- Seq(_0, _1)
      } yield (State("b"), _0, out, d, State("c"))
    } else Seq()


//    m.addTransitions(step2_2)
//    m.execute()

    Nil
  }

//  val m: TM = new Seq(
//    ("a", )
//  )
}


