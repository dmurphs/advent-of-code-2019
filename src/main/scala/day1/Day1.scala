package day1

import scala.io.Source
import scala.util.Using

object Day1 {
  def getFuelRequired(mass: Int): Int = {
    val fuelRequired = (mass / 3) - 2
    if (fuelRequired <= 0) 0 else fuelRequired + getFuelRequired(fuelRequired)
  }

  def main() {
    Using(Source.fromFile("src/main/scala/day1/input.txt")) { source =>
      val lines = source.getLines.toList
      val fuelRequired = lines.map(line => getFuelRequired(line.toInt)).sum
      println(fuelRequired)
    }
  }
}
