package day3

import day3.Day3.recordCoordinates

import scala.io.Source
import scala.util.Using

sealed trait Instruction {
  val length: Int
}
case class R(length: Int) extends Instruction
case class L(length: Int) extends Instruction
case class U(length: Int) extends Instruction
case class D(length: Int) extends Instruction

object Day3 {
  def getWirePaths: List[List[Instruction]] = {
    Using(Source.fromFile("src/main/scala/day3/input.txt")) { source =>
      val lines = source.getLines
      lines.map(line => line.split(",").map(encodedInstruction => {
        val encodedDirection = encodedInstruction.head
        val encodedLength = encodedInstruction.tail
        val length = encodedLength.toInt
         encodedDirection match {
            case 'R' => R(length)
            case 'L' => L(length)
            case 'U' => U(length)
            case 'D' => D(length)
          }
      }).toList).toList
    }.get
  }

  def getVisitedCoordinates(instruction: Instruction, currentCoordinate: (Int, Int)): List[(Int, Int)] = {
    val length = instruction.length
    val (x, y) = currentCoordinate
    val getUpdatedX = (xCoord: Int) => (xCoord, y)
    val getUpdatedY = (yCoord: Int) => (x, yCoord)

    val visited = instruction match {
      case _: R => (x + 1 to x + length).map(getUpdatedX)
      case _: L => (x - length until x).reverse.map(getUpdatedX)
      case _: U => (y + 1 to y + length).map(getUpdatedY)
      case _: D => (y - length until y).reverse.map(getUpdatedY)
    }
    visited.toList
  }

  def recordCoordinates(path: List[Instruction], startCoordinates: (Int, Int) = (0, 0)): List[(Int, Int)] = {
    path match {
      case instruction :: remaining =>
        val visited = getVisitedCoordinates(instruction, startCoordinates)
        val nextStart = visited.last
        visited.concat(recordCoordinates(remaining, nextStart))
      case _ => List.empty
    }
  }

  def manhattanDistanceFromOrigin(coords: (Int, Int)): Int = {
    val (x2, y2) = coords
    y2.abs + x2.abs
  }

  def findIntersections(): Unit = {
    val path1 :: path2 :: _ = getWirePaths
    val path1Coordinates = recordCoordinates(path1)
    val path2Coordinates = recordCoordinates(path2)

    val intersections = path1Coordinates.intersect(path2Coordinates)
    val intersectionsWithManhattanDistances = intersections
      .map(intersection => (intersection, manhattanDistanceFromOrigin(intersection)))
    println(intersectionsWithManhattanDistances
      .minBy({ case (_, distance) => distance }))
  }
}
