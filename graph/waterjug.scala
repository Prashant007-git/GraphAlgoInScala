object Task_1_1 extends App {
  import scala.collection.mutable
  type Pii = (Int, Int)
  def print_path(mp: mutable.Map[Pii, Pii], u: Pii): Unit = {
    if (u == (0, 0)) {
      println("0 0")
      return
    }
    print_path(mp, mp(u))
    println(s"${u._1} ${u._2}")
  }

  def BFS(a: Int, b: Int, target: Int): Unit = {
    val m = mutable.Map.empty[Pii, Int]
    var isSolvable = false
    val mp = mutable.Map.empty[Pii, Pii]
    val q = mutable.Queue.empty[Pii]

    q.enqueue((0, 0))
    while (q.nonEmpty) {
      val u = q.dequeue()
      if (m.getOrElse(u, 0) == 1)
        ()
      else if (u._1 > a || u._2 > b || u._1 < 0 || u._2 < 0)
        ()
      else {
        m += u -> 1
        if (u._1 == target || u._2 == target) {
          isSolvable = true
          print_path(mp, u)
          if (u._1 == target) {
            println(s"${u._1} 0")
            println(s"0 ${u._1}")
          } else if (u._2 == target && u._1 != 0)
            println(s"0 ${u._2}")
          return
        }
        // filling 2nd jug completely
        if (!m.contains(u._1, b)) {
          q.enqueue((u._1, b))
          mp += (u._1, b) -> u
        }
        // filling 1st jug completely
        if (!m.contains(a, u._2)){
          q.enqueue((a, u._2))
          mp += (a, u._2) -> u
        }
        // pouring from 1st jug to 2nd jug
        val d1 = b - u._2
        if (u._1 >= d1) {
          val c1 = u._1 - d1
          if (!m.contains(c1, b)) {
            q.enqueue((c1, b))
            mp += (c1, b) -> u
          }
        }
        else {
          val c2 = u._1 + u._2
          if (!m.contains(0, c2)){
            q.enqueue((0, c2))
            mp += (0, c2) -> u
          }
        }
        // pouring from 2nd jug to 1st jug
        val d2 = a - u._1
        if (u._2 >= d2) {
          val c3 = u._2 - d2
          if (!m.contains(a, c3)) {
            q.enqueue((a, c3))
            mp += (a, c3) -> u
          }
        }
        else {
          val c4 = u._1 + u._2
          if (!m.contains(c4, 0)) {
            q.enqueue((c4, 0))
            mp += (c4, 0) -> u
          }
        }
        // emptying 2nd jug
        if (!m.contains(u._1, 0)) {
          q.enqueue((u._1, 0))
          mp += (u._1, 0) -> u
        }
        // emptying 1st jug
        if (!m.contains(0, u._2)) {
          q.enqueue((0, u._2))
          mp += (0, u._2) -> u
        }
      }
    }
    if (!isSolvable) {
      println("Solution not possible")
    }
  }
  val Jug1 = 3
  val Jug2 = 4
  val target = 2
  println("Path from initial state to solution state:")
  BFS(Jug1, Jug2, target)
}
