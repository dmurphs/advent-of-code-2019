package utils

object Utils {

  implicit class ExtendedList[T](val value: List[T]) {
    def safeIndex(index: Int): Option[T] = if (index >= 0 && this.value.length > index)
      Option(this.value(index))
      else Option.empty

    def safeUpdate(index: Int, value: T): Option[List[T]] = if (index >= 0 && this.value.length > index)
      Option(this.value.updated(index, value))
      else Option.empty
  }

  implicit class ExtendedString(val value: String) {
    def padLeft(length: Int, c: Char): String = {
      val numPadChars = length - this.value.length
      if (numPadChars <= 0)
        this.value
        else s"${c.toString * numPadChars}$value"
    }
  }
}
