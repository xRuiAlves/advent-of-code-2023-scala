package util

object MathUtils {
  def gcd(n1: Long, n2: Long): Long =
    if (n2 == 0) n1
    else gcd(n2, n1 % n2)

  def lcm(n1: Long, n2: Long): Long = Math.abs(n1 * n2) / gcd(n1, n2)
}
