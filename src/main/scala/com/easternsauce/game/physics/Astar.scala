package com.easternsauce.game.physics

import com.easternsauce.game.EnhancedChainingSyntax.enhancedScalaUtilChainingOps
import com.easternsauce.model.Vec2
import com.softwaremill.quicklens._

import scala.annotation.tailrec
import scala.collection.immutable.Map

object Astar {
  def generatePathingGraph(terrain: PhysicsWorld): Map[Vec2, PathingNode] = {
    val elems =
      for (x <- 0 until terrain.widthInTiles; y <- 0 until terrain.heightInTiles)
        yield Vec2(x, y) -> PathingNode(Vec2(x, y), terrain.clearances.getOrElse(Vec2(x, y), Int.MaxValue))

    val pathingNodes: Map[Vec2, PathingNode] = elems.toMap

    def tryAddingEdge(
      pathingNodes: Map[Vec2, PathingNode],
      terrain: PhysicsWorld,
      fromX: Int,
      fromY: Int,
      toX: Int,
      toY: Int,
      weight: Float
    ): Map[Vec2, PathingNode] = {
      if (0 <= toY && toY < terrain.heightInTiles && 0 <= toX && toX < terrain.widthInTiles) {
        if (terrain.traversables(Vec2(fromX, fromY)) && terrain.traversables(Vec2(toX, toY))) {
          val targetNode = pathingNodes(Vec2(toX, toY))
          pathingNodes.updated(Vec2(fromX, fromY), pathingNodes(Vec2(fromX, fromY)).addEdge(weight, targetNode))
        } else pathingNodes
      } else pathingNodes
    }

    val straightWeight = 10f
    val diagonalWeight = 14.1421356237f

    (for {
      x <- 0 until terrain.widthInTiles
      y <- 0 until terrain.heightInTiles
    } yield (x, y)).foldLeft(pathingNodes) {
      case (pathingNodes, (x, y)) =>
        pathingNodes
          .pipe(tryAddingEdge(_, terrain, x, y, x - 1, y, straightWeight))
          .pipe(tryAddingEdge(_, terrain, x, y, x + 1, y, straightWeight))
          .pipe(tryAddingEdge(_, terrain, x, y, x, y - 1, straightWeight))
          .pipe(tryAddingEdge(_, terrain, x, y, x, y + 1, straightWeight))
          .pipeIf(
            x - 1 >= 0 && y - 1 >= 0
              && terrain.traversables(Vec2(x - 1, y)) && terrain.traversables(Vec2(x, y - 1))
          )(tryAddingEdge(_, terrain, x, y, x - 1, y - 1, diagonalWeight))
          .pipeIf(
            x + 1 < terrain.widthInTiles && y - 1 >= 0
              && terrain.traversables(Vec2(x + 1, y)) && terrain.traversables(Vec2(x, y - 1))
          )(tryAddingEdge(_, terrain, x, y, x + 1, y - 1, diagonalWeight))
          .pipeIf(
            x - 1 >= 0 && y + 1 < terrain.heightInTiles
              && terrain.traversables(Vec2(x - 1, y)) && terrain.traversables(Vec2(x, y + 1))
          )(tryAddingEdge(_, terrain, x, y, x - 1, y + 1, diagonalWeight))
          .pipeIf(
            x + 1 < terrain.widthInTiles && y + 1 < terrain.heightInTiles
              && terrain.traversables(Vec2(x + 1, y)) && terrain.traversables(Vec2(x, y + 1))
          )(tryAddingEdge(_, terrain, x, y, x + 1, y + 1, diagonalWeight))
    }

  }

  // caution: heavy computational load!
  def findPath(terrain: PhysicsWorld, startPos: Vec2, finishPos: Vec2, capability: Int): List[Vec2] = {
    val startTilePos = terrain.getClosestTile(startPos)
    val finishTilePos = terrain.getClosestTile(finishPos)

    val freshAstarGraph = Astar
      .getAstarGraph(terrain.pathingGraph)
      .modify(_.at(startTilePos))
      .using(_.modify(_.g).setTo(0))

    val astarState =
      AstarState(
        astarGraph = freshAstarGraph,
        openSet = Set(startTilePos),
        closedSet = Set(),
        finishPos = finishTilePos,
        foundPath = false
      )

    @tailrec
    def traverse(astarState: AstarState): AstarState = {
      if (astarState.openSet.nonEmpty && !astarState.foundPath) {
        val currentNode = astarState.astarGraph(astarState.openSet.minBy {
          case Vec2(x, y) => astarState.astarGraph(Vec2(x, y)).f
        })
        val resultingAstarState = if (currentNode.pos == finishTilePos) {
          astarState.modify(_.foundPath).setTo(true)
        } else {
          val updatedAstarState = astarState
            .modify(_.openSet)
            .setTo(astarState.openSet - currentNode.pos)
            .modify(_.closedSet)
            .setTo(astarState.closedSet + currentNode.pos)
          currentNode.pathingNode.outgoingEdges
            .foldLeft(updatedAstarState) {
              case (astarState, PathingEdge(weight, neighborPos)) =>
                processNeighbor(astarState, currentNode.pos, neighborPos, weight)
            }
        }

        traverse(resultingAstarState)
      } else {
        astarState
      }
    }

    def processNeighbor(
      astarState: AstarState,
      originNodePos: Vec2,
      neighborPos: Vec2,
      distanceBetweenNodes: Float
    ): AstarState = {
      if (
        astarState.closedSet
          .contains(neighborPos) || Astar.calculateHeuristic(originNodePos, astarState.finishPos) >= 60 && terrain
          .clearances(neighborPos) < capability
      ) {
        astarState
      } else {
        val originNode = astarState.astarGraph(originNodePos)
        val neighbor = astarState.astarGraph(neighborPos)

        val tentativeGscore = originNode.g + distanceBetweenNodes
        neighbor match {
          case node if !astarState.openSet.contains(node.pos) =>
            val updatedNode = node
              .modify(_.h)
              .setTo(Astar.calculateHeuristic(node.pos, astarState.finishPos))
              .modify(_.parent)
              .setTo(Some(originNode.pos))
              .modify(_.g)
              .setTo(tentativeGscore)
              .pipe(node => node.modify(_.f).setTo(node.g + node.h))

            astarState
              .modify(_.astarGraph)
              .using(_.updated(node.pos, updatedNode))
              .modify(_.openSet)
              .setTo(astarState.openSet + node.pos)
          case node if tentativeGscore < node.g =>
            val updatedNode = node
              .modify(_.parent)
              .setTo(Some(originNode.pos))
              .modify(_.g)
              .setTo(tentativeGscore)
              .pipe(node => node.modify(_.f).setTo(node.g + node.h))

            astarState.modify(_.astarGraph).setTo(astarState.astarGraph.updated(node.pos, updatedNode))
          case _ => astarState
        }
      }
    }

    val result = traverse(astarState)

    val lastNode = result.astarGraph(result.finishPos)

    def reconstructPath(lastNode: AstarNode): List[Vec2] = {
      if (lastNode.parent.nonEmpty) {
        lastNode.pos :: reconstructPath(result.astarGraph(lastNode.parent.get))
      } else List()
    }

    reconstructPath(lastNode).reverse.map(terrain.getTileCenter)
  }

  def getAstarGraph(pathingGraph: Map[Vec2, PathingNode]): Map[Vec2, AstarNode] = {
    pathingGraph.view.mapValues(AstarNode(_)).toMap
  }

  def calculateHeuristic(startPos: Vec2, finishPos: Vec2): Double = {
    (Math.abs(finishPos.x - startPos.x) + Math.abs(finishPos.y - startPos.y)) * 10
  }

}

case class PathingNode(pos: Vec2, clearance: Int, outgoingEdges: List[PathingEdge] = List()) {
  def addEdge(weight: Float, node: PathingNode): PathingNode = {
    val newEdge = PathingEdge(weight, node.pos)
    PathingNode(pos, clearance, newEdge :: outgoingEdges)
  }

  override def toString: String = "(" + pos.x + ", " + pos.y + ":" + outgoingEdges.size + ")"
}

case class PathingEdge(weight: Float, neighborPos: Vec2)

case class AstarNode(
  pathingNode: PathingNode,
  parent: Option[Vec2] = None,
  f: Double = Double.MaxValue,
  g: Double = Double.MaxValue,
  h: Double = Double.MaxValue
) {
  def pos: Vec2 = pathingNode.pos
}

case class AstarState(
  astarGraph: Map[Vec2, AstarNode],
  openSet: Set[Vec2],
  closedSet: Set[Vec2],
  finishPos: Vec2,
  foundPath: Boolean
)
