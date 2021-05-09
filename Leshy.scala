import math._
import scala.util._
import scala.io.StdIn._
import scala.util.matching.Regex
import scala.annotation.tailrec


object Player extends App {
  import IOUtils._

  val defaultCells = parseCells()

  // game loop
  while(true) {
    implicit val gameState = parseGameState(defaultCells)

    gameState.day match {
      case d if d < 11 => opening
      case d if d < 19 => middlegame
      case _ => lategame
    }
  }


  def opening(implicit gameState: GameState): Unit = {
    def tryToSeed: Boolean = {
      if (gameState.seedCost > 0) {
        return false
      }

      val seedActions = gameState.possibleMoves.flatMap {
        case s: Seed if wontDisturbAllyTrees(s.destinationIndex) => Some(s)
        case _ => None
      }

      if (seedActions.isEmpty) {
        return false
      } else {
        seedActions.maxBy(s => s.destinationCell.richness)
          .execute("I need more!")
        return true
      }
    }

    if (!performGreedyGrow() && !tryToSeed) {
      Wait().execute("Zzz...")
    }
  }

  def middlegame(implicit gameState: GameState): Unit = {
    val numberOfOwnedLargeTrees = gameState.cells.map { cell =>
      cell.tree.map(t => if (t.size == Large && t.isMine) 1 else 0).getOrElse(0)
    }.sum

    if (numberOfOwnedLargeTrees > 6 && completeLifecycle()) {
      return 
    }

    def tryToSeed: Boolean = {
      if (gameState.seedCost > 0) {
        return false
      }

      val seedActions = gameState.possibleMoves.flatMap {
        case s: Seed => Some(s)
        case _ => None
      }

      if (seedActions.isEmpty) {
        return false
      } else {
        seedActions.maxBy(s => s.destinationCell.richness)
          .execute("I need more!")

        return true
      }
    }

    if (!tryToSeed && !performGreedyGrow()) {
      Wait().execute("Zzz...")
    }
  }

  def lategame(implicit gameState: GameState): Unit = {
    val numberOfOwnedLargeTrees = gameState.cells.map { cell =>
      cell.tree.map(t => if (t.size == Large && t.isMine) 1 else 0).getOrElse(0)
    }.sum

    if ((gameState.day == 23 || numberOfOwnedLargeTrees > 4) && completeLifecycle()) {
      return
    }

    if (!performGreedyGrow()) {
      Wait().execute("Zzz...")
    }
  }
  

  def completeLifecycle()(implicit gameState: GameState): Boolean = {
    val completeActions = gameState.possibleMoves.flatMap {
      case c: Complete => Some(c)
      case _ => None
    }

    if (completeActions.isEmpty) {
      return false
    } else {
      completeActions.maxBy(c => c.cell.richness)
        .execute("Chop, chop, chop!")
      return true
    }
  }

  // def performCheapestGrow()(implicit gameState: GameState): Boolean = {
  //   val growActions = gameState.possibleMoves.flatMap {
  //     case g: Grow => Some(g)
  //     case _ => None
  //   }

  //   if (growActions.isEmpty) {
  //     return false
  //   }

  //   val minCost = growActions.map(_.cost).min

  //   growActions.filter(_.cost == minCost)
  //     .maxBy(g => g.cell.richness)
  //     .execute("Grow, grow, grow!")
      
  //   return true
  // }

  def performGreedyGrow()(implicit gameState: GameState): Boolean = {
    val growActions = gameState.possibleMoves.flatMap {
      case g: Grow => Some(g)
      case _ => None
    }

    if (growActions.isEmpty) {
      return false
    }

    growActions.maxBy(g => g.cell.tree.map(_.size).get)
      .execute("Grow, grow, grow!")

    return true
  }

  def wontDisturbAllyTrees(index: Int)(implicit gameState: GameState): Boolean = {
    @tailrec def pathInDirectionIsClear(dir: Int, currentIndex: Int, limit: Int): Boolean = {
      val disturbance = gameState.cells(currentIndex)
        .tree
        .map(_.isMine)
        .getOrElse(false)

      val nextHop = defaultCells(currentIndex).neighbors(dir)

      if (disturbance) {
        return false
      }
      else if (nextHop == -1 || limit == 0) {
        return true
      }
      else {
        return pathInDirectionIsClear(dir, nextHop, limit - 1)
      }
    }

    (0 to 5).forall(i => pathInDirectionIsClear(dir = i, currentIndex = index, limit = 3))
  }

  // def countOccupiedNeighbors(index: Int)(implicit gameState: GameState): Int = {
  //   @tailrec def countInDirection(dir: Int, currentIndex: Int, limit: Int, acc: Int): Int = {
  //     val occupied = gameState.cells(currentIndex).tree.isDefined

  //     val nextAcc = if (occupied) acc + 1 else acc
  //     val nextHop = defaultCells(currentIndex).neighbors(dir)

  //     if (limit = 0) {
  //       return nextAcc
  //     } else {
  //       return countInDirection(dir, nextHop, limit - 1, nextAcc)
  //     }
  //   }

  //   (0 to 0).map(i => countInDirection(dir = i, currentIndex = index, limit = 3, acc = 0)).sum
  // }
}

sealed abstract class Richness(val ord: Int) extends Ordered[Richness] {
  override def compare(that: Richness): Int = this.ord.compare(that.ord)
}
  case object Unusable      extends Richness(0)
  case object LowQuality    extends Richness(1)
  case object MediumQuality extends Richness(2)
  case object HighQuality   extends Richness(3)

case class Cell(richness: Richness, neighbors: Vector[Int], tree: Option[Tree])

sealed abstract class Size(val ord: Int) extends Ordered[Size] {
  override def compare(that: Size): Int = this.ord.compare(that.ord)
}
  case object Root   extends Size(0)
  case object Small  extends Size(1)
  case object Medium extends Size(2)
  case object Large  extends Size(3)

case class Tree(
  cellIndex: Int,
  size: Size,
  isMine: Boolean,
  isDormant: Boolean,
)

case class GameState(
  day: Int,
  nutrients: Int,
  sun: Int,
  score: Int,
  oppSun: Int,
  oppScore: Int,
  oppIsWaiting: Boolean,
  cells: Vector[Cell],
  possibleMoves: Vector[Action],
  seedCost: Int,
)

sealed abstract class Action {
  def execute(debugMsg: String = ""): Unit
}

object Action {
  val growSignature: Regex = """GROW (\d+)""".r
  val seedSignature: Regex = """SEED (\d+) (\d+)""".r
  val completeSignature: Regex = """COMPLETE (\d+)""".r
  val waitSignature: Regex = """WAIT""".r
}

case class Grow(index: Int, cell: Cell, cost: Int) extends Action {
  override def execute(debugMsg: String = ""): Unit = {
    println(s"GROW $index $debugMsg")
  }
}

case class Seed(
  sourceIndex: Int,
  destinationIndex: Int,
  sourceCell: Cell,
  destinationCell: Cell,
) extends Action {
  override def execute(debugMsg: String = ""): Unit = {
    println(s"SEED $sourceIndex $destinationIndex $debugMsg")
  }
}

case class Complete(index: Int, cell: Cell) extends Action {
  override def execute(debugMsg: String = ""): Unit = {
    println(s"COMPLETE $index $debugMsg")
  }
}

case class Wait() extends Action {
  override def execute(debugMsg: String = ""): Unit = {
    println(s"WAIT $debugMsg")
  }
}

object IOUtils {
  def parseCells(): Vector[Cell] =
    Vector.tabulate(readLine().toInt) { _ =>
      val Array(index, richness, neigh0, neigh1, neigh2, neigh3, neigh4, neigh5) = (readLine() split " ").map (_.toInt)

      Cell(
        richness match {
          case 0 => Unusable
          case 1 => LowQuality
          case 2 => MediumQuality
          case 3 => HighQuality
          case _ => throw new IllegalArgumentException()
        },
        neighbors = Vector(neigh0, neigh1, neigh2, neigh3, neigh4, neigh5),
        tree = None
      )
    }

  def parseGameState(defaultCells: Vector[Cell]): GameState = {
    val day = readLine().toInt // the game lasts 24 days: 0-23
    val nutrients = readLine().toInt

    val Array(sun, score) = (readLine() split " ").map (_.toInt)

    val Array(_oppSun, _oppScore, _oppIsWaiting) = readLine() split " "
    val oppSun = _oppSun.toInt
    val oppScore = _oppScore.toInt
    val oppIsWaiting = _oppIsWaiting.toInt != 0

    val trees: Vector[Tree] = Vector.tabulate(readLine().toInt) { _ =>
      val Array(_cellIndex, _size, _isMine, _isDormant) = readLine() split " "

      Tree(
        cellIndex = _cellIndex.toInt,
        size = _size.toInt match {
          case 0 => Root
          case 1 => Small
          case 2 => Medium
          case 3 => Large
          case _ => throw new IllegalArgumentException()
        },
        isMine = _isMine.toInt != 0,
        isDormant = _isDormant.toInt != 0,
      )
    }

    val growCost: Map[Size, Int] = Map(
      Root -> (1 + trees.count(t => t.size == Small  && t.isMine)),
      Small -> (3 + trees.count(t => t.size == Medium && t.isMine)),
      Medium -> (7 + trees.count(t => t.size == Large  && t.isMine)),
    )

    val seedCost: Int = trees.count(t => t.isMine && t.size == Root)

    val indexedTrees = trees.groupBy(_.cellIndex).view.mapValues(_.head).toMap
    val cells = defaultCells.zipWithIndex.map {
      case (cell, index) => cell.copy(tree = indexedTrees.get(index))
    }

    val possibleMoves = Vector.tabulate(readLine().toInt) { _ =>
      readLine() match {
        case               Action.growSignature(index) => Grow(index.toInt, cells(index.toInt), growCost(cells(index.toInt).tree.get.size))
        case Action.seedSignature(source, destination) => Seed(source.toInt, destination.toInt, cells(source.toInt), cells(destination.toInt))
        case           Action.completeSignature(index) => Complete(index.toInt, cells(index.toInt))
        case                    Action.waitSignature() => Wait()
        case _ => throw new IllegalArgumentException()
      }
    }

    GameState(day, nutrients, sun, score, oppSun, oppScore, oppIsWaiting, cells, possibleMoves, seedCost)
  }
}