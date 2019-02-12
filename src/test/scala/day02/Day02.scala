import org.scalatest._
import day02.Day02

class Day02FirstPuzzleTests extends FlatSpec{
    val input_exercise: List[String] = List[String](
        "abcdef",
        "bababc",
        "abbcde",
        "abcccd",
        "aabcdd",
        "abcdee",
        "ababab"
    )
    assert(Day02.solve_first(input_exercise) == 12)
}

class Day02SecondPuzzleTests extends FlatSpec {
    val input_exercise: List[String] = List[String](
        "abcde",
        "fghij",
        "klmno",
        "pqrst",
        "fguij",
        "axcye",
        "wvxyz",
    )
    assert(Day02.solve_second(input_exercise) == "fgij")
}
