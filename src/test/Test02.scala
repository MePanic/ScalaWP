package test

import scala.Array.canBuildFrom

object Test02 {

  
  def rle1(x:Array[Byte]):List[Pair[Int, Int]] = {
    
    var res:List[Pair[Int, Int]] = List()

    if(x == null || x.size == 0){res = List()}
    else {
          var akt:Int = x.apply(0)
    var count:Int = 0
      
      for(b <- x) yield {
    	if(b==akt){count+=1}
    	else {res:+=(akt,count); akt = b; count = 1}
    }
    res:+=(akt,count)}
    res
  }
    
    def rle2(x:Array[Byte]):List[Int] = {
    
    var res:List[Int] = List()

    if(x == null || x.size == 0){res = List()}
    else {
      
          var akt:Int = x.apply(0)
    var count:Int = 0
      for(b <- x) yield {
    	if(b==akt){count+=1}
    	else {res:+=akt;res:+=count; akt = b; count = 1}
    }
    res:+=akt;res:+=count}
    res
  }
  
  
  def main(args: Array[String]){
    val bArr1= Array[Byte](0,0,0,1,1,0,1)
    val bArr2= Array[Byte](1,1,0,1,0,0,0,0,1,1,0,1,1,1,1,1)
    println(rle1(bArr1))
    println(rle1(bArr2))
    println(rle2(bArr1))
    println(rle2(bArr2))
    println(rle1(Array[Byte]()))
    println(rle2(Array[Byte]()))
    println(rle1(null))
    println(rle2(null))
  }
  
}