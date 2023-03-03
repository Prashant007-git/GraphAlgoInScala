import scala.collection.mutable
object Task_2 extends App{
  case class State(leftBank: (Int, Int), rightBank: (Int, Int), boatOnLeftBank: Boolean)
  val solution = solve()
  if (solution.isDefined) {
    println("Solution found:")
    printSolution(solution.get)
  } else {
    println("No solution found.")
  }
  def generateSuccessors(state: State): Seq[State] = {
    val (leftM, leftC) = state.leftBank
    val (rightM, rightC) = state.rightBank
    var successors = Seq[State]()
    for {
      m <- 0 to 2
      c <- 0 to 2
      if m + c >= 1 && m + c <= 2
      if (m == 0 || m >= c) && (rightM - m == 0 || rightM - m >= rightC - c)
    } {
      if (state.boatOnLeftBank) {
        successors = successors :+ State((leftM - m, leftC - c), (rightM + m, rightC + c), boatOnLeftBank = false)
      }
      else {
        successors = successors :+ State((leftM + m, leftC + c), (rightM - m, rightC - c), boatOnLeftBank = true)
      }
    }
    successors
  }
  def solve(): Option[List[State]] = {
    val initialState = State((3, 3), (0, 0), boatOnLeftBank = true)
    val goalState = State((0, 0), (3, 3), boatOnLeftBank = false)
    var visited = Set(initialState)
    val queue = mutable.Queue(List(initialState))
    while (queue.nonEmpty) {
      val path = queue.dequeue()
      val currentState = path.last
      if (currentState == goalState) {
        return Some(path)
      }
      val successors = generateSuccessors(currentState).filterNot(visited.contains) // filter out any state object that is already visited
      visited ++= successors  // ++= used to add state objects of successors because += only adds one element
      queue ++= successors.map(path :+ _) // adds all state objects of successors to explore each state with the current path list
    }
    None
  }
  def printSolution(solution: List[State]): Unit = {
    solution.zipWithIndex.foreach { case (state, index) =>
      println(s"Step $index:")
      println(s"Left bank: ${state.leftBank._1} missionaries, ${state.leftBank._2} cannibals")
      println(s"Right bank: ${state.rightBank._1} missionaries, ${state.rightBank._2} cannibals")
      val boatLocation = if (state.boatOnLeftBank) "left bank" else "right bank"
      println(s"Boat is on the $boatLocation")
      println()
    }
  }
}
