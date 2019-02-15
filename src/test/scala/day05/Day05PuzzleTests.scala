package day05

import org.scalatest._

class Day05PuzzleTests extends FlatSpec{
    val input_exercise: String = "dabAcCaCBAcCcaDA"

    assert(Day05.solve_first("aA") == "")
    assert(Day05.solve_first("abBA") == "")
    assert(Day05.solve_first("abAB") == "abAB")
    assert(Day05.solve_first("aabAAB") == "aabAAB")
    assert(Day05.solve_first("XXABCDEedcbaXX") == "XXXX")
    assert(Day05.solve_first("XXABCDEedEFGgfecbaBbBXX") == "XXBXX")
    assert(Day05.solve_first("AAAAAAAAAAA") == "AAAAAAAAAAA")
    assert(Day05.solve_first("AAAAAAAAAAAA") == "AAAAAAAAAAAA")
    assert(Day05.solve_first("abcdqwertyZz") == "abcdqwerty")
    assert(Day05.solve_first("bBbbbbBb") == "bbbb")
    assert(Day05.solve_first("cAaaC") == "caC")
    assert(Day05.solve_first(input_exercise) == "dabCBAcaDA")
    //assert(Day05.solve_second(input_exercise) == 99 * 45)
}
