import org.scalatest._
import day03.Day03

class Day03FirstPuzzleTests extends FlatSpec{
    val input_exercise: List[String] = List[String](
        "#1 @ 1,3: 4x4",
        "#2 @ 3,1: 4x4",
        "#3 @ 5,5: 2x2",
    )
    assert(Day03.solve_first(input_exercise) == 4)

    assert(Day03.solve_second(input_exercise) == 3)
}
