package test

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.TIMEOUT

object Aufg7_1 extends App {

  class realFuncEval extends Actor {
    var func = (a: Double) => 2.0 * a: Double
    def act {
      loop {
        receive {
//          case s: (String => String) => reply(Double.NaN)
          case f: RealFunction => func = f
          case 'DONE =>
          case 'EXIT => exit
          case d: Double => reply(func(d))
          case i: Int => reply(func(i))
          case l: Long => reply(func(l))
          case f: Float => reply(func(f))
          case _ => reply(Double.NaN)
        }
      }
    }
  }

  // RealFunction ist ein Synonym für Double => Double
  type RealFunction = Double => Double
  // beispielsweise:
  val dSquare: RealFunction = (x: Double) => x * x
  val realFunctionEvaluator = new realFuncEval
  realFunctionEvaluator.start

  val acting = actor {

    realFunctionEvaluator ! dSquare
    realFunctionEvaluator ! 5
    realFunctionEvaluator ! 7L
    realFunctionEvaluator ! 10F
    // Fehlerhafte Argumente werden mit NaN beantwortet
    realFunctionEvaluator ! "Hallo"
    realFunctionEvaluator ! 15.0
    // ’DONE steht für: Berechnung mit dieser Funktion beendet!
    // realFunctionEvaluator ! ’DONE
    realFunctionEvaluator ! { (x: Double) => 1.0 / x }
    realFunctionEvaluator ! 5
    realFunctionEvaluator ! 7L
    realFunctionEvaluator ! 10F
    // Fehlerhafte Argument
    //    realFunctionEvaluator ! { (s: String) => "Hallo" + s }
    realFunctionEvaluator ! 0.0
    // realFunctionEvaluator ! ’DONE
    // Unangenehm: falsche Funktion erwartet ...
    //    realFunctionEvaluator ! { (s: String) => "Hallo" + s }
    realFunctionEvaluator ! 1.0
    // realFunctionEvaluator ! ’EXIT
    loop {
      reactWithin(100) {
        case r: Double => println(r)
        case s: String => println(s)
        case TIMEOUT => println("ende"); exit
      }
    }
  }

  acting.start

}