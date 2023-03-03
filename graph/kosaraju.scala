object Kosa_raju_scc extends App{
  import scala.collection.mutable
  class Graph(val vertices: Int) {
    var graph = Map[Int, List[Int]]().withDefaultValue(Nil)
    def add_edge(u: Int, v: Int): Unit = {
      graph = graph + (u -> (v :: graph(u)))
    }
    def DFSUtil(v: Int, visited: mutable.ArrayBuffer[Boolean]): Unit = {
      visited(v) = true
      print(v + " ")
      for (i <- graph(v)) {
        if (!visited(i)) {
          DFSUtil(i, visited)
        }
      }
    }
    def fillOrder(v: Int, visited: mutable.ArrayBuffer[Boolean], stack: mutable.ListBuffer[Int]): Unit = {
      visited(v) = true
      for (i <- graph(v)) {
        if (!visited(i)) {
          fillOrder(i, visited, stack)
        }
      }
      stack += v
    }
    def getTranspose(): Graph = {
      val g = new Graph(vertices)
      for ((k, v) <- graph) {
        for (j <- v) {
          g.add_edge(j, k)
        }
      }
      g
    }
    def printSCCs(): Unit = {
      val stack = mutable.ListBuffer[Int]()
      val visited = mutable.ArrayBuffer.fill[Boolean](vertices)(false)
      for (i <- 0 until vertices) {
        if (!visited(i)) {
          fillOrder(i, visited, stack)
        }
      }
      val gr = getTranspose()
      for (i <- 0 until vertices) {
        visited(i) = false
      }
      while (stack.nonEmpty) {
        val i = stack.remove(stack.length - 1)
        if (!visited(i)) {
          gr.DFSUtil(i, visited)
          println()
        }
      }
    }
  }
  val g = new Graph(5)
  g.add_edge(1, 0)
  g.add_edge(0, 2)
  g.add_edge(2, 1)
  g.add_edge(0, 3)
  g.add_edge(3, 4)
  println("Following are strongly connected components in given graph:")
  g.printSCCs()
}
