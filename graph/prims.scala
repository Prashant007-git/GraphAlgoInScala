object Prims_algo extends App{
  val INF: Int = 9999999
  val V: Int = 5
  var result = 0
  val G: Array[Array[Int]] = Array(
    Array(0, 9, 75, 0, 0),
    Array(9, 0, 95, 19, 42),
    Array(75, 95, 0, 51, 66),
    Array(0, 19, 51, 0, 31),
    Array(0, 42, 66, 31, 0)
  )
  val selected: Array[Boolean] = Array(false, false, false, false, false)
  var no_edge: Int = 0
  selected(0) = true
  println("Edge : Weight\n")
  while (no_edge < V - 1) {
    var minimum: Int = INF
    var x: Int = 0
    var y: Int = 0
    for (i <- 0 until V) {
      if (selected(i)) {
        for (j <- 0 until V) {
          if ((!selected(j)) && G(i)(j) != 0) {
            if (minimum > G(i)(j)) {
              minimum = G(i)(j)
              x = i
              y = j
            }
          }
        }
      }
    }
    println(s"$x-$y:   ${G(x)(y)}")
    selected(y) = true
    no_edge += 1
    result = result + G(x)(y)
  }
  println(s"minimum cost of this spanning tree is $result")
}
