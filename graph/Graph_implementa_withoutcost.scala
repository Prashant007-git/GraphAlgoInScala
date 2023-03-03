object Graph_adj_list_without_cost extends App{
  class Graph() {
    var adj_list = Map[Int, List[Int]]().withDefaultValue(Nil)

    def add_edge(u: Int, v: Int): Unit = {
      adj_list = adj_list + (u -> (v :: adj_list(u)))
    }

    def print_graph(): Unit = {
      for (i <- adj_list.keys) {
        print(i + "-> ")
        for (j <- adj_list(i)) {
          print(j + " ")
        }
        println()
      }
    }
  }

  val graph = new Graph()
  graph.add_edge(1, 2)
  graph.add_edge(2, 1)
  graph.add_edge(1, 0)
  graph.add_edge(0, 1)
  graph.add_edge(1, 3)
  graph.add_edge(3, 1)
  graph.add_edge(0, 3)
  graph.add_edge(3, 0)
  graph.add_edge(3, 2)
  graph.add_edge(2, 3)
  graph.add_edge(4, 3)
  graph.add_edge(3, 4)
  graph.add_edge(4, 0)
  graph.add_edge(0, 4)
  graph.print_graph()
}
