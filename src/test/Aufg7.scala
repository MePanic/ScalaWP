package test

import scala.actors.Actor
import scala.actors.Actor._

object Aufg7 extends Actor {

  def main(args: Array[String]) {

    // RealFunction ist ein Synonym für Double => Double
    type RealFunction = Double => Double
    // beispielsweise:
    val dSquare: RealFunction = (x: Double) => x*x
    val realFunctionEvaluator = actor {
      // ihr Code ...
    }

    actor {

      realFunctionEvaluator!dSquare
      realFunctionEvaluator ! 5
      realFunctionEvaluator ! 7L
      realFunctionEvaluator ! 10F
      // Fehlerhafte Argumente werden mit NaN beantwortet
      realFunctionEvaluator ! "Hallo"
      realFunctionEvaluator ! 15.0
      // ’DONE steht für: Berechnung mit dieser Funktion beendet!
//      realFunctionEvaluator!’DONE
      realFunctionEvaluator ! { (x: Double) => 1.0 / x }
      realFunctionEvaluator ! 5
      realFunctionEvaluator ! 7L
      realFunctionEvaluator ! 10F
      // Fehlerhafte Argument
      realFunctionEvaluator ! { (s: String) => "Hallo" + s }
      realFunctionEvaluator ! 0.0
//      realFunctionEvaluator!’DONE
      // Unangenehm: falsche Funktion erwartet ...
      realFunctionEvaluator ! { (s: String) => "Hallo" + s }
      realFunctionEvaluator ! 1.0
//      realFunctionEvaluator!’EXIT
      loop {
        reactWithin(100) {
          case r: Double => println(r)
          case Error(s) => println(s)
          case TIMEOUT => println("ende"); exit
        }
      }
    }

  }

}