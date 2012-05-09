package test

import test.TimeImplicits.num2Time

object Main {
  import TimeImplicits._
  
  trait Time{
    def value: Long ;  def toString : String
    def +(t2: Time): Time = this match {
      case z0:TimeInMin => t2 match {
          case s: TimeInSec =>  TimeInSec(value*60 +t2.value)
          case ms: TimeInMilliSec => TimeInMilliSec(value*1000*60+t2.value)
          case micr: TimeIn_μ_Sec  => TimeIn_μ_Sec(value*1000*1000*60+t2.value)
          case n: TimeInNanoSec  => TimeInNanoSec(value*1000*1000*1000*60+t2.value)
          case m: TimeInMin => TimeInMin(value + t2.value)
          case _ => NaT }
      case z: TimeInSec => t2 match {
          case s: TimeInSec =>  TimeInSec(value +t2.value)
          case ms: TimeInMilliSec => TimeInMilliSec(value*1000+t2.value)
          case micr: TimeIn_μ_Sec  => TimeIn_μ_Sec(value*1000*1000+t2.value)
          case n: TimeInNanoSec  => TimeInNanoSec(value*1000*1000*1000+t2.value)
          case m: TimeInMin => TimeInSec(value + t2.value*60)
          case _ => NaT }
      case z1: TimeInMilliSec => t2 match {
          case s: TimeInSec =>  TimeInMilliSec(value +t2.value*1000)
          case m: TimeInMilliSec => TimeInMilliSec(value+t2.value)
          case micr: TimeIn_μ_Sec  => TimeIn_μ_Sec(value*1000+t2.value)
          case n: TimeInNanoSec  => TimeInNanoSec(value*1000*1000+t2.value)
          case m: TimeInMin => TimeInMilliSec(value + t2.value*60*1000)
          case _ => NaT }
      case z2: TimeIn_μ_Sec => t2 match {
          case s: TimeInSec =>  TimeIn_μ_Sec(value +t2.value*1000*1000)
          case m: TimeInMilliSec => TimeIn_μ_Sec(value+t2.value*1000)
          case micr: TimeIn_μ_Sec  => TimeIn_μ_Sec(value + t2.value)
          case n: TimeInNanoSec  => TimeInNanoSec(value*1000+t2.value)
          case m: TimeInMin => TimeIn_μ_Sec(value +t2.value*1000*1000*60)
          case _ => NaT }
      case z3: TimeInNanoSec => t2 match {
          case s: TimeInSec =>  TimeInNanoSec(value +t2.value*1000*1000*1000)
          case m: TimeInMilliSec => TimeInNanoSec(value+t2.value*1000*1000)
          case micr: TimeIn_μ_Sec  => TimeInNanoSec(value + t2.value*1000)
          case n: TimeInNanoSec  => TimeInNanoSec(value+t2.value)
          case m: TimeInMin => TimeInNanoSec(value +t2.value*1000*1000*1000*60)
          case _ => NaT }
      case _ => NaT
    }  
  }
    
  class TimeFactory(val value : Long){
    def s = { TimeInSec(value) }
    def ms ={ TimeInMilliSec(value)}
    def ns ={TimeInNanoSec(value)}
    def μs ={TimeIn_μ_Sec(value)}
    def m = {TimeInMin(value)}
  }
  case object NaT extends Time{
    val value : Long = Double.NegativeInfinity.toLong
    def ==(other: Time)= false
    override def toString= "NaT"
  }
  object TimeInSec{
    def apply(value: Long): Time = {
      if(value >= 0) new TimeInSec(value)
      else NaT }
  }

  class TimeInSec private(val value: Long) extends Time{
    override def toString:String = ""+value+" sec"
  }

    object TimeInMin{
    def apply(value: Long): Time = {
      if(value >= 0) new TimeInMin(value)
      else NaT }
  }

  class TimeInMin private(val value: Long) extends Time{
    override def toString:String = ""+value+" min"
  }
  
  object TimeInMilliSec{
    def apply(value: Long): Time = {
      if(value >= 0) new TimeInMilliSec(value)
      else NaT}
  }

  class TimeInMilliSec private(val value: Long) extends Time{
    override def toString:String = ""+value+" msec"
  }

  object TimeIn_μ_Sec{
    def apply(value: Long): Time = {
      if(value >= 0) new TimeIn_μ_Sec(value)
      else NaT }
  }

  class TimeIn_μ_Sec private(val value: Long) extends Time{
    override def toString:String = ""+value+" μsec"
  }

  object TimeInNanoSec{
    def apply(value: Long): Time = {
      if(value >= 0) new TimeInNanoSec(value)
      else NaT }
  }

  class TimeInNanoSec private(val value: Long) extends Time{
    override def toString:String = ""+value+" nsec"
  }

  def Σ(arguments : Time*):Time={
    arguments.reduceLeft((x,y) => x+y)
  }
  
  def test01 {
    println(10 ns)
    println(20 μs)
    println(30 ms)
    println(40 s)
    println(50 m)
    println(-1.s)
    
    
    
    println(10.s)
    println(20.ms)
    println(10.s + 20.ms)
    println(20.ms + 10.s)
    println(1.ns + 50.ns + 20.μs + 3.ms +4.s)
    println(3.ms + 4.s + 1.ns + 20.μs +50.ns) // das ist ein μ
    println( Σ(3.ms,4.s,1.ns,20.μs,50.ns))
    // 10.sec.sec <- compiliert nicht
    // 10.sec+10 <- compiliert nicht
    println((-1).s)
    println(Σ(3.ms,4.s,1.ns,NaT,20.μs,50.ns))
//     
  }

  def main(args: Array[String]): Unit = {
    test01
  }

}