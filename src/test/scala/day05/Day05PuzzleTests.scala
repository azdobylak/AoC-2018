package day05

import org.scalatest._

class Day05PuzzleTests extends FlatSpec{
    val input_exercise: String = "dabAcCaCBAcCcaDA"

    assert(Day05.reduce("aA") == "")
    assert(Day05.reduce("abBA") == "")
    assert(Day05.reduce("abAB") == "abAB")
    assert(Day05.reduce("aabAAB") == "aabAAB")
    assert(Day05.reduce("XXABCDEedcbaXX") == "XXXX")
    assert(Day05.reduce("XXABCDEedEFGgfecbaBbBXX") == "XXBXX")
    assert(Day05.reduce("AAAAAAAAAAA") == "AAAAAAAAAAA")
    assert(Day05.reduce("AAAAAAAAAAAA") == "AAAAAAAAAAAA")
    assert(Day05.reduce("abcdqwertyZz") == "abcdqwerty")
    assert(Day05.reduce("bBbbbbBb") == "bbbb")
    assert(Day05.reduce("cAaaC") == "caC")
    assert(Day05.reduce(input_exercise) == "dabCBAcaDA")

    assert(Day05.reduce(input_exercise, 'a') == "dbCBcD")
    assert(Day05.reduce(input_exercise, 'b') == "daCAcaDA")
    assert(Day05.reduce(input_exercise, 'c') == "daDA")
    assert(Day05.reduce(input_exercise, 'd') == "abCBAc")
    assert(Day05.solve_second(input_exercise) == 4)
}
