package day05

import org.scalatest._

class Day05PuzzleTests extends FlatSpec{
    val input_exercise: String = "dabAcCaCBAcCcaDA"

    assert(Day05.solve_first(input_exercise) == "dabCBAcaDA")
    //assert(Day05.solve_second(input_exercise) == 99 * 45)
}
