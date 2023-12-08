package util

object MathUtils {
  def gcd(n1: Long, n2: Long): Long =
    if (n2 == 0) n1
    else gcd(n2, n1 % n2)

  def lcm(n1: Long, n2: Long): Long = Math.abs(n1 * n2) / gcd(n1, n2)

  def lcm(numbers: Array[Long]): Long = {
    require(numbers.length >= 2, "At least 2 numbers required to calculate the least common multiple!!")
    numbers.reduce((a, b) => lcm(a, b))
  }
}
