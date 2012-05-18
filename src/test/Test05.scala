package test

object Test05 extends App{

  def count[N<:AnyVal](args:N*):Tuple4[Int,Int,Int,Int] = args.foldLeft((0,0,0,0))((a,b) => b match {
    case _:Boolean => (a._1,a._2+1,a._3,a._4)
    case _:Unit => (a._1+1,a._2,a._3,a._4)
    case _:Double => (a._1,a._2,a._3,a._4+1)
    case _:Float => (a._1,a._2,a._3,a._4+1)
    case _ => (a._1,a._2,a._3+1,a._4)
  })
  
  println(count(true,(),(),false,'a',1.0,2,true,2L,1F,10:Byte,300:Short))
}