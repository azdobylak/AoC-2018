package day07

import java.net.URL

import scala.io.Source
import scala.collection.{SortedSet, mutable}


case class Worker(var sleepTime: Int,
                  var task: Option[Char] = None,
                  var secondsToFinishTask: Int = 0) {

    def finishTask(): Unit = this.task = None

    def setTask(task: Char): Unit = {
        this.task = Some(task)
        this.secondsToFinishTask = this.sleepTime + (task - 'A' + 1)
    }

    def work(): Unit = this.secondsToFinishTask -= 1

    def isBusy: Boolean = this.secondsToFinishTask > 0
    def hasFinished: Boolean = this.task.isEmpty
}


object Day07 {
    val input: URL = day07.Day07.getClass.getResource("input")
    val lines: List[String] = Source.fromURL(input).getLines().toList

    def main(args: Array[String]): Unit = {
        println(solve_first(lines))
        println(solve_second(lines))
    }

    def parseInput(line: String): (Char, Char) = {
        val pattern = "Step ([a-zA-Z]) must be finished before step ([a-zA-Z]) can begin.".r
        val matches = pattern.findAllIn(line)
        (matches.group(1)(0), matches.group(2)(0))
    }

    def traversePseudoTree(charPairs: List[(Char, Char)], charAdditionalProcessingTime: Int, workersNum: Int = 1): (String, Int) = {

        def traverse(tasksMap: List[(Char, Char)], tasksOrder: String): SortedSet[Char] = {
            val tasksLeft = tasksMap.filterNot(p => (tasksOrder contains p._1) && (tasksOrder contains p._2))
            val blockedTasks: Set[Char] =
                tasksLeft.collect{ case pair if !(tasksOrder contains pair._1) => pair._2 }.toSet
            val tasksPool = tasksLeft.flatten{case (c1, c2) => List(c1, c2)}.toSet diff tasksOrder.toSet

            val availableTasks: SortedSet[Char] = SortedSet.empty[Char] ++ (tasksPool diff blockedTasks)

            availableTasks
        }

        val workers: List[Worker] = List.fill[Worker](workersNum)(Worker(charAdditionalProcessingTime))
        var result: String = ""
        var timeElapsed: Int = 0
        val blockedChars: mutable.Set[Char] = mutable.Set.empty[Char]

        var wasTurnIdle = true
        do{
            wasTurnIdle = true
            for(worker <- workers) {
                if(!worker.isBusy){
                    // finish task if there is any
                    worker.task match {
                        case Some(c: Char) => {
                            result += c
                            blockedChars.remove(c)
                            worker.finishTask()
                        }
                        case None =>
                    }
                }
                if(!worker.isBusy){
                    // try to pick new task
                    val chars: SortedSet[Char] =  traverse(charPairs, result)
                    val freeChars = chars diff blockedChars
                    if(freeChars.nonEmpty) {
                        val c = freeChars.head
                        worker.setTask(c)
                        blockedChars.add(c)
                        wasTurnIdle = false
                    }
                }
                if (worker.isBusy){
                    worker.work()
                    wasTurnIdle = false
                }
            }
            timeElapsed += 1
        } while(!wasTurnIdle)

        (result, timeElapsed - 1)
    }

    def solve_first(lines: List[String]): String = {
        val charPairs: List[(Char, Char)]  = lines.map(l => parseInput(l))
        val (tasks, _) = traversePseudoTree(charPairs, 0, 1)
        tasks
    }

    def solve_second(lines: List[String], processingTime: Int = 60, workersNum: Int = 5): Int = {
        val charPairs: List[(Char, Char)]  = lines.map(l => parseInput(l))
        val (_, time) = traversePseudoTree(charPairs, processingTime, workersNum)
        time
    }
}