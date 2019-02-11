import org.scalatest._
import day02.Day02

class Day02Tests extends FlatSpec{
    val input_exercise: List[String] = List[String](
        "abcdef",
        "bababc",
        "abbcde",
        "abcccd",
        "aabcdd",
        "abcdee",
        "ababab"
    )
    assert(Day02.solve(input_exercise) == 12)
}