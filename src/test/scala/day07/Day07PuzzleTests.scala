package day07

import org.scalatest._

class Day07PuzzleTests extends FlatSpec{
    val input_exercise: List[String] = List[String](
        "Step C must be finished before step A can begin.",
        "Step C must be finished before step F can begin.",
        "Step A must be finished before step B can begin.",
        "Step A must be finished before step D can begin.",
        "Step B must be finished before step E can begin.",
        "Step D must be finished before step E can begin.",
        "Step F must be finished before step E can begin.",
    )

    assert(Day07.solve_first(input_exercise) == "CABDFE")
}
