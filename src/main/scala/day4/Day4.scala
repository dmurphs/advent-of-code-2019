package day4

object Day4 {

  def isValidV1(num: Int): Boolean ={
    val digits = num.toString.map((c: Char) => c.asDigit)
    val pairs = digits.sliding(2).map(x => (x.head, x.tail.head)).toList

    pairs.exists({ case (a, b) => a == b }) && pairs.forall({ case (a, b) => a <= b})
  }

  def accumulateDigitsWithConsecutiveCounts(maybePrevious: Option[(Int, List[Int])], digit: Int): Option[(Int, List[Int])] = {
      maybePrevious.flatMap({ case (previousDigit: Int, consecutiveCounts: List[Int]) => {
        if (digit < previousDigit) {
          Option.empty
        } else {
          val updatedConsecutiveCounts = if (digit == previousDigit) {
            consecutiveCounts.updated(consecutiveCounts.length - 1, consecutiveCounts.last + 1)
          } else {
            consecutiveCounts.appended(1)
          }
          Option((digit, updatedConsecutiveCounts))
        }
      }})
  }

  def isValidV2(num: Int): Boolean ={
    val firstDigit :: remaining = num.toString.map((c: Char) => c.asDigit).toList

    val accumulator = Option((firstDigit, List(1)))
    remaining.foldLeft(accumulator)(accumulateDigitsWithConsecutiveCounts).exists {
      case (_, consecutiveCounts: List[Int]) => consecutiveCounts.contains(2)
    }
  }

  def findValid() = {
    val range = 130254 to 678275

    val validForV1 = range.filter(isValidV1)
    println(validForV1.length)

    val validForV2 = range.filter(isValidV2)
    println(validForV2.length)
  }

}
