package test

import Main.TimeFactory

object TimeImplicits {
  
  implicit def num2Time(num:Long):TimeFactory = new TimeFactory(num)
  
}


object TimeUnit extends Enumeration {
  
  type TimeUnit = Value
  val noTime,nanosecond,microsecond,millisecond,second,minute = Value
  
}