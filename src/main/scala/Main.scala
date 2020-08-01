import scala.collection.mutable

object Main extends App {
  // transition tuple : *(State, Input, Output, Direction, NewState)*
  sealed trait Direction
  case object R extends Direction
  case object L extends Direction

  // tape alphabet
  sealed trait Symbol
  case object _0 extends Symbol { override def toString: String = "0" }
  case object _1 extends Symbol { override def toString: String = "1" }

  object State { implicit val ord = Ordering.by(unapply) }
  case class State(s: String)

  type CurrState = State
  type NextState = State

  type Input = Symbol
  type Output = Symbol

  type Transition = (CurrState, Input, Output, Direction, NextState)

  /*
   * how many TMs with 2 states over size=2 alphabet?
   * # unique transitions:
   * 2 * 2 * 2 * 2 * 2 = 32
   *
   * how many transitions total?
   */

  /*

  Definition 1. A Turing machine is a quadruple (Q ∪ {z}, Γ, δ, a) where
  • z is a distinguished state called a halting state
  • Γ is the tape alphabet
  • δ is a partial function from Q × Γ to Q ∪ {z} × Γ × {l, r} called the transition function
  • a ∈ Q is a distinguished state called the start state

  */

  object Tape {
    // on second thought, using arrays was a mistake...
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

    /*
     * it's kinda ugly: arrNeg doesn't use element at index 0...
     * (pos < 0 goes to arrNeg, pos >= 0 goes to arrPos)
     */
    var pos = 0
    var arrPos = mkArr(64)
    var arrNeg = mkArr(64) // TODO no sense in allocating tape size that is greater than max execution steps

    private def arr: Array[Symbol] = if (pos >= 0) arrPos else arrNeg

    def curr: Symbol = arr.apply(math.abs(pos))

    def isEmpty: Boolean = arrPos.forall(_ == _0) && arrNeg.forall(_ == _0)

    def setAndMove(s: Symbol, d: Direction): Tape = {
      arr.update(math.abs(pos), s)
      pos += (if (d ==  R) 1 else -1)
      if (pos >= 0)
        arrPos = fitToSize(arrPos, math.abs(pos))
      else
        arrNeg = fitToSize(arrNeg, math.abs(pos))
      this
    }

    def copy(): Tape = {
      val res = new Tape
      res.pos = pos
      java.lang.System.arraycopy(arrPos, 0, res.arrPos, 0, arrPos.length)
      java.lang.System.arraycopy(arrNeg, 0, res.arrNeg, 0, arrNeg.length)
      res
    }

    override def toString: String = {
      /*
       * example output:
       * 0 0 0 0 1* 0 0 1 |1| 0 1 0 1 0

       * the |1| denotes starting position of tape head
       * the * denotes current tape (head) position
       */
      val x = arrPos.lastIndexWhere(_ == _1)
      val y = arrNeg.lastIndexWhere(_ == _1)
      val posi = math.min(if (x == -1) 2 else x + 2, arrPos.length - 1)
      val negi = math.min(if (y == -1) 2 else y + 2, arrNeg.length - 1)

      (
        (negi to 1 by -1).map { i => s"""${arrNeg(i)}${if (i * -1 == pos) "*" else ""}""" } ++ // "1" because arrNeg(0) isn't used
        (0 to posi).map { i => s"""${if (i == 0) s"|${arrPos(i)}|" else s"${arrPos(i)}"}${if (i == pos) "*" else ""}""" }
      ).mkString(" ")
    }
  }

  type TMTransitions = Map[(CurrState, Input), (Output, Direction, NextState)]

  // TODO
  //   - don't copy tape every time
  case class TM(
    n: Int = 5,
    m: Int = 2,  // TODO m is a poor choice, used as var name for machines...
    transitions: Map[(CurrState, Input), (Output, Direction, NextState)] = Map(
      (State("a"), _0) -> (_1, R, State("b"))  // step 1
    ),
    tape: Tape = new Tape,
    var state: State = State("a"),
    var steps: Int = 0,
  ) {
    lazy val states: Set[State] = transitions.flatMap { case ((s, _), (_, _, ns)) => Seq(s, ns) }.toSet
    lazy val symbols: Set[Symbol] = transitions.flatMap { case ((_, i), (o, _, _)) => Seq(i, o) }.toSet

    lazy val id: String = transitions.map { case ((s, i), (o, d, ns)) => s"${s.s}$i->$o$d${ns.s}" }.mkString(", ")

    def transitionsStr: String = transitions.map { case ((s, i), (o, d, ns)) => s"${s.s}$i->$o$d${ns.s}" }.mkString(", ")
    override def toString: String = s"TM(state=${state.s}, $tape, steps=$steps, [$transitionsStr])"

    // creates a new TM !!
    def withTransitions(ts: Transition*): TM = copy(
      transitions = transitions ++ ts.map { t => (t._1, t._2) -> (t._3, t._4, t._5)}.toMap,
      tape = tape.copy(),
    )

    def nStateFull: Boolean = states.size == n
    def mSymbolFull: Boolean = symbols.size == m

    /*
     * there are n transitions of the form (_, 0, _, _, _) and
     * all of them are of the form         (_, 0, _, R, _)
     */
    def zeroDextrousWith(t: Transition): Boolean = {
      val ts = transitions ++ Map((t._1, t._2) -> (t._3, t._4, t._5))
      val x = ts.filter { case ((_, _0), (_, _, _)) => true; case _ => false }
      x.size == n && x.forall { case ((_, _0), (_, R, _)) => true; case _ => false }
    }

    // returns current state and input if there is no transition defined for it, none otherwise
    def step(): Option[(CurrState, Input)] = {
      transitions.get((state, tape.curr)) match {
        case Some((out, dir, ns)) =>
          state = ns
          tape.setAndMove(out, dir)
          steps += 1
          None

        // "an undefined combination of state S (state) and input I (tape.curr) is found"
        case None => Some((state, tape.curr))
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

  //def generateHelp(m: Iterable[TM]): Iterable[TM] = ???

  val q = mutable.Queue.empty[TM]
  val output = mutable.ArrayBuffer.empty[TM]

  /*
   * don't generate machines with same transformations.
   * save hashes of transitions string of TM.
   */
  val seen = mutable.HashSet.empty[String]
  val seenOut = mutable.HashSet.empty[String]

  // returns true if m wasn't seen before
  def addTM_Q(m: TM): Unit =
    if (seen.add(m.id)) q.addOne(m)

  def outputTM(m: TM): Unit = {
    if (seenOut.add(m.id)) output.addOne(m)
  }

  def discard(m: TM, n: Int): Boolean = {
    m.steps > 0 &&
    (
      // TODO checking for empty tape every step is kinda expensive / easily optimized
      m.tape.isEmpty ||                        // violates empty tape condition
      //(m.state == State("z") && m.steps <= n)  // M halted and activity <= n
      m.state == State("z")  // my idea: machines that reach halting state during generation are not worth looking at
                             // since we limit execution steps to well below 4k, and they can't possible output enough
                             // 1's to be competitive
    )
  }

  // TODO:
  //  - m (tape alphabet size) is hardcoded to 2 atm
  //  - memory optimization (generate states once, various collections setc)
  //  - parallel generation of machines
  def generate(n: Int): Iterable[TM] = {
    (
      (
        for {
          ns <- Seq(State("a"), State("b"))
          out <- Seq(_0, _1)
        } yield TM().withTransitions((State("b"), _0, out, L, ns))
      )
      ++
      (
        if (n >= 3) {
          for {
            d <- Seq(R, L)
            o <- Seq(_0, _1)
          } yield TM().withTransitions((State("b"), _0, o, d, State("c")))
        } else Seq()
      )
    ).foreach(addTM_Q)

    while (q.nonEmpty) {
      val m = q.dequeue()
      println(s">>>> DEQUEUED $m")

      var outdone = false
      var break = false
      while (m.steps < 50 && !discard(m, n) && !outdone && !break) {
        m.step() match {
          case None => // step made successfully
            //println(s">>>> AFTER STEP: $m")

          case Some((s, i)) => // hit a state/input combo that has no transition defined for it
            // TODO dunno not sure at all! this is one of the worst fucking pieces of pseudo code i've seen
            // fucking PROLOG LMAO
            break = true
            if (
              (m.nStateFull && m.mSymbolFull && !m.zeroDextrousWith((s, i, _1, R, State("z")))) ||
              (m.transitions.size == m.n * m.m - 1) // not checking 0-dextrous here? ¯\_(ツ)_/¯, also wtf is this condition
            ) {
              println(s">>>> OUTPUT on state exhaustion criteria: ${m.transitionsStr}")
              outputTM(m.withTransitions((s, i, _1, R, State("z"))))
              outdone = true
            } else {
              // TODO bug with duplicate machines
              (
                for {
                  ns <- if (m.states.size == n) m.states
                        else m.states ++ Set(State((m.states.max.s(0).toInt + 1).toChar.toString))
                  o <- Seq(_0, _1)
                  d <- Seq(R, L)
                  t = (s, i, o, d, ns)
                  if !m.zeroDextrousWith(t)
                } yield m.withTransitions(t)
              ).foreach(addTM_Q)
            }
        }
      }

      if (discard(m, n)) {
        println(s">>>> DISCARD ${m.transitionsStr}")
      } else if (m.steps >= 50 && !outdone) {
        println(s">>>> OUTPUT on max execution limit: ${m.transitionsStr}")
        outputTM(m)
      }
    }

    println(s">>>> FINAL OUTPUT (SIZE = ${output.size}): $output")
    Nil
  }

  println(s">>>> RUNNING WITH $args")
  generate(args(0).toInt)
//  val m: TM = new Seq(
//    ("a", )
//  )
}


