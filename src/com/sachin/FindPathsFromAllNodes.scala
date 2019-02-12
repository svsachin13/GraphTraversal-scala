package com.sachin

object FindPathsFromAllNodes extends App {

  val inputNodes = List(Node("a", "x"), Node("a", "b"), Node("b", "c"), Node("c", "d"), Node("b", "y"), Node("b", "z"), Node("d", "a"))

  def findPaths(allNodes: List[Node], newNode: Node, path: List[Node] = Nil, isVisited: List[String] = Nil, allPaths: List[List[Node]] = Nil): List[List[Node]] = {
    if (isVisited.contains(newNode.dest)) {
      allPaths ++ List(path)
    } else {
      val nextNodes = allNodes.filter(_.source == newNode.dest)
      if (nextNodes.isEmpty) {
        allPaths ++ List(path :+ newNode)
      } else if (nextNodes.size > 1) {
        nextNodes.flatMap { node =>
          findPaths(allNodes, node, path :+ newNode, isVisited :+ newNode.source)
        }
      } else {
        findPaths(allNodes, nextNodes.head, path :+ newNode, isVisited :+ newNode.source)
      }
    }
  }

  inputNodes.flatMap(node => findPaths(inputNodes, node)).foreach(println)

}
