package day2

import scala.io.Source
import scala.util.Using
import utils.Utils.ExtendedList

object Day2 {
  @scala.annotation.tailrec
  def runProgram(program: List[Int], start: Int = 0): Either[String, List[Int]] = {
    val opcode = program(start)

    opcode match {
      case 1 | 2 => {
        val operation = if (opcode == 1)
          (a: Int, b: Int) => a + b
          else (a: Int, b: Int) => a * b

        val updatedProgramResult = for (
          firstIndex <- program.safeIndex(start + 1);
          secondIndex <- program.safeIndex(start + 2);
          indexToUpdate <- program.safeIndex(start + 3);
          firstValue <- program.safeIndex(firstIndex);
          secondValue <- program.safeIndex(secondIndex);
          updated <- program.safeUpdate(indexToUpdate, operation(firstValue, secondValue))
        ) yield updated

        if (updatedProgramResult.isDefined) {
          runProgram(updatedProgramResult.get, start + 4)
        } else Left(s"Invalid instruction at $start")
      }
      case 99 => Right(program)
      case _  => Left(s"Encountered invalid opcode at $start")
    }
  }

  def programFromFile: List[Int] = {
    Using(Source.fromFile("src/main/scala/day2/input.txt")) { source =>
      source.getLines.mkString.trim.split(",")
        .map(num => num.toInt)
        .toList
    }.get
  }

  def partOne(): Unit = {
    val result = runProgram(programFromFile)
    println(result)
  }

  @scala.annotation.tailrec
  def findMagicPairRec(program: List[Int], pairs: Seq[(Int, Int)], currentIndex: Int = 0): Option[(Int, Int)] = {
    if (currentIndex >= pairs.length) {
      Option.empty
    } else {
      val (a, b) = pairs(currentIndex)
      val updatedProgram = program
        .updated(1, a)
        .updated(2, b)
      val result = runProgram(updatedProgram)
      result match {
        case Right(output) if (output.nonEmpty && output(0) == 19690720) => Option((a, b))
        case _             => findMagicPairRec(program, pairs, currentIndex + 1)
      }
    }
  }

  def partTwo(): Unit = {
    val possibleAddressValues = 0 to 99
    val pairs = for (a <- possibleAddressValues; b <- possibleAddressValues) yield (a, b)
    val magicNumber: Option[Int] = for (
      (a, b) <- findMagicPairRec(programFromFile, pairs)
    ) yield a * 100 + b
    println(magicNumber)
  }
}
