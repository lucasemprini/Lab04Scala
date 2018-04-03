package u09lab

/**
  * Graph example from Unit 05
  */

trait Graph {
  type Node   // Node and Edge are abstract types, member of Graph
  type Edge
  def addEdge(n1: Node, n2: Node, e: Edge): Unit
  def nodes: Set[Node]
  def outEdges(n: Node): Set[Edge]
  def inEdges(n: Node): Set[Edge]
}

// an implementation, still leaving Node and Edge as abstract
abstract class GraphImpl() extends Graph with GraphProperties {
  protected var data = Set[(Node,Edge,Node)]()
  override def addEdge(n1: Node, n2: Node, e: Edge) = data += ((n1,e,n2))
  override def nodes = (data map(_._1)) ++ (data map(_._3))
  override def outEdges(n: Node) = data filter (_._1 == n) map (_._2)
  override def inEdges(n: Node) = data filter (_._3 == n) map (_._2)

  override def edgeBetween(n1: Node, n2: Node): Boolean = data.exists(e => e._1==n1 && e._3==n2)

  def nodesReachedByEdge(e: Edge) = data filter (_._2 == e) map (_._3)

  def pathBetween(n1: Node, n2: Node) = pathBetween(n1, n2, Set())

  private def pathBetween(n1: Node, n2: Node, visitedNodes: Set[Node]): Boolean = {
    val outs: Set[Edge] = outEdges(n1)
    if(inEdges(n2).intersect(outs).size>0) true
    else {
      outs.exists { e =>
        val reachedNodes = nodesReachedByEdge(e)
        reachedNodes.exists(n => pathBetween(n, n2, visitedNodes+n))
      }
    }
  }

  def isComplete: Boolean =
    (for(n1 <- nodes; n2 <- nodes; if n1!=n2) yield edgeBetween(n1,n2)).reduce(_&&_)

  def isConnected: Boolean =
    (for(n1 <- nodes; n2 <- nodes; if n1!=n2) yield pathBetween(n1,n2)).reduce(_&&_)

  def hasLoops = data.exists(e => e._1==e._3)
}

trait GraphProperties { self: Graph =>
  def edgeBetween(n1: Node, n2: Node): Boolean
  def pathBetween(n1: Node, n2: Node): Boolean
  def isComplete: Boolean
  def isConnected: Boolean
  def hasLoops: Boolean
}

class BasicGraph extends GraphImpl with GraphProperties {
  type Node = String
  type Edge = Int
}

object TryGraph extends App {
  val g = new BasicGraph
  g.addEdge("a","b",1)
  g.addEdge("c","d",2)
  g.addEdge("a","d",3)
  println(g.nodes,g.outEdges("a"),g.inEdges("d")) //(Set(a, c, b, d),Set(1, 3),Set(2, 3))
  println(g.edgeBetween("a","d")) // true
  println(g.edgeBetween("d","a")) // false
  println(g.isComplete) // false
  println(g.isConnected) // false
}