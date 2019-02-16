package day06

import org.scalatest._

class Day06PuzzleTests extends FlatSpec{
    val input_exercise: List[String] = List[String](
        "1, 1",
        "1, 6",
        "8, 3",
        "3, 4",
        "5, 5",
        "8, 9",
    )

    assert(Day06.solve_first(input_exercise) == 17)
    assert(Day06.solve_second(input_exercise, 32) == 16)
}
