object BFS_Traversal extends App {
  val graph = new Graph(5)
  graph.add_edge(1, 2)
  graph.add_edge(2, 1)
  graph.add_edge(1, 3)
  graph.add_edge(3, 1)
  graph.add_edge(2, 3)
  graph.add_edge(2, 4)
  graph.add_edge(4, 2)
  graph.add_edge(5, 2)
  graph.add_edge(4, 5)
  graph.add_edge(3, 4)
  graph.bfs(1)
  import scala.collection.mutable
  class Graph(numVertex: Int){
    var adj_list = Map[Int,List[Int]]().withDefaultValue(Nil)

    def add_edge(u: Int,v:Int): Unit = {
      adj_list = adj_list + (u -> (v:: adj_list(u)))
    }
    def bfs(start: Int): Unit = {
      val visited = Array.fill[Boolean](numVertex+1)(false)
      val queue = mutable.Queue[Int]()
      visited(start) = true
      queue.enqueue(start)
      while(queue.nonEmpty){
        val vertex = queue.dequeue()
        println(vertex)
        for(i <- adj_list(vertex)){
          if(!visited(i)){
            visited(i) = true
            queue.enqueue(i)
          }
        }
      }
    }
  }
}
