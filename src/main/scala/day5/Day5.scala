package day5

import scala.io.Source
import scala.util.Using
import utils.Utils.ExtendedList
import utils.Utils.ExtendedString

object Day5 {
  def evaluateParameter(index: Int, mode: Char, program: List[Int]): Option[Int] = {
    val valueAtIndex = program.safeIndex(index)
    valueAtIndex.flatMap(value =>
      mode match {
        case '0' => program.safeIndex(value)
        case '1' => Option(value)
        case _   => Option.empty
      }
    )
  }

  @scala.annotation.tailrec
  def runProgram(program: List[Int], start: Int = 0): Either[String, List[Int]] = {
    val instructionMetadata = program(start).toString.padLeft(5, '0')
    val secondParameterMode = instructionMetadata.charAt(1)
    val firstParameterMode = instructionMetadata.charAt(2)
    val opcode = instructionMetadata.slice(3, 5)

    opcode match {
      case "01" | "02" => {
        val operation = if (opcode == "01")
          (a: Int, b: Int) => a + b
          else (a: Int, b: Int) => a * b

        val updatedProgramResult = for (
          firstValue <- evaluateParameter(start + 1, firstParameterMode, program);
          secondValue <- evaluateParameter(start + 2, secondParameterMode, program);
          indexToUpdate <- program.safeIndex(start + 3);
          updated <- program.safeUpdate(indexToUpdate, operation(firstValue, secondValue))
        ) yield updated

        if (updatedProgramResult.isDefined) {
          runProgram(updatedProgramResult.get, start + 4)
        } else Left(s"Invalid add/multiply instruction at $start")
      }
      case "03" => {
        val rawInput = scala.io.StdIn.readLine("Enter input: ")
        val input = rawInput.toInt

        val updatedProgram = for (
          index <- program.safeIndex(start + 1);
          updatedProgram <- program.safeUpdate(index, input)
        ) yield updatedProgram

        if (updatedProgram.isDefined)
          runProgram(updatedProgram.get, start + 2)
          else Left(s"Invalid save instructions at $start")
      }
      case "04" => {
        val value = for (
          index <- program.safeIndex(start + 1);
          valueAtIndex <- program.safeIndex(index)
        ) yield valueAtIndex

        if (value.isDefined) {
          println(value.get)
          runProgram(program, start + 2)
        } else Left(s"Invalid show instructions at $start")
      }
      case "05" | "06" => {
        val parameters = for (
          firstValue <- evaluateParameter(start + 1, firstParameterMode, program);
          secondValue <- evaluateParameter(start + 2, secondParameterMode, program)
        ) yield (firstValue, secondValue)

        if (parameters.isDefined) {
          val (firstValue: Int, secondValue: Int) = parameters.get
          if (opcode == "05" && firstValue != 0 || opcode == "06" && firstValue == 0)
            runProgram(program, secondValue)
            else runProgram(program, start + 3)
        } else Left(s"Invalid jump instruction at $start")
      }
      case "07" | "08" => {
        val getUpdatedProgram = (firstValue: Int, secondValue: Int, indexToUpdate: Int) => {
          if (opcode == "07" && firstValue < secondValue || opcode == "08" && firstValue == secondValue)
            program.safeUpdate(indexToUpdate, 1)
          else program.safeUpdate(indexToUpdate, 0)
        }

        val updatedProgram = for (
          firstValue <- evaluateParameter(start + 1, firstParameterMode, program);
          secondValue <- evaluateParameter(start + 2, secondParameterMode, program);
          indexToUpdate <- program.safeIndex(start + 3);
          updated <- getUpdatedProgram(firstValue, secondValue, indexToUpdate)
        ) yield updated

        if (updatedProgram.isDefined) {
          runProgram(updatedProgram.get, start + 4)
        } else Left(s"Invalid less than/equals instruction at $start")
      }
      case "99" => Right(program)
      case _  => Left(s"Encountered invalid opcode: $opcode at $start")
    }
  }

  def programFromFile: List[Int] = {
    Using(Source.fromFile("src/main/scala/day5/input.txt")) { source =>
      source.getLines.mkString.trim.split(",")
        .map(num => num.toInt)
        .toList
    }.get
  }

  def partOne(): Unit = {
    val result = runProgram(programFromFile)
    println(result)
  }
}
