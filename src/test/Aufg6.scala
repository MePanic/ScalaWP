package test

import scala.math._

object Aufg6 extends App {

  trait ~>[A, B] extends (A => B) {
    var count:Int
    def applyOrDefault(a: A, default: => B): B
    def apply(x: A): B = applyOrDefault(x, throw new MatchError())
  }

  def use1[A, B](f: A => B)(x: A) = f(x)
  def use2[A, B](f: A ~> B)(default: B)(x: A) /*...*/ = f.applyOrDefault(x, default)
  
  val f = new (Double ~> Double)/*~>[Double, Double]*/{
    var count = 0
    def applyOrDefault(x: Double, default: => Double):Double = {
      count += 1
      if (x >= 0) math.sqrt(x)
      else Double.NaN
    }
  }
  
  println(use1(f)(2))
  println(use1(f)(-2.0))
  println(use1(f)(-2.0))
  println(use2(f)(0.0)(-2.0))
  println(f.count)
}