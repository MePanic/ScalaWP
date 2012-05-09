package test

object Test00 {
	
  case class Position(val x:Int=0,val y:Int=0){
    require(x >= 0 & y >= 0, "x und y muessen groesser/gleich Null sein!")

    override def toString() = "Position("+x+","+y+")"
      
    
  }
  
  def main(args: Array[String]){
    
    println(Position(3,4))
    val p1=Position()
    println(p1.x + "," + p1.y)
    val p2=Position(3)
    println(p2.x + "," + p2.y)
    println(Position(-1,4))
  }
  
}