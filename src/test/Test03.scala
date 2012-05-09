package test

import scala.Option.option2Iterable
import scala.annotation.tailrec

object Test03 {

  def main(args:Array[String]){
    val lst = List(Some("Hello"), None, Some(1), Some("World"))
    val lst2 = List(Some(1), Some(2), None)
    val lst3 = List(Some("Hallo"), Some("Welt"))
    listPrinter(lst)
    println(sequenceOption(lst2))
    println(sequenceOption(lst3))
    println(blub(2))
    println(blub(-2))
    println(blub2(2))
    println(blub2(-2))
    println(exp(2,4))
    println(exp(2,-4))
    println(exp(3,2))
    println(exp(3,-1))
    println(exp(9,30))
    println(exp(9,-30))
    println(exp(9,-30).left get)

  }
  
  def listPrinter(lst:List[Option[Any]]){
    for(i <- lst){
      i match{
        case None => println("None")
        case Some(i:Int) => println("Int: " + i)
        case Some(i:String) => println("String: " + i)
        case _ => println("Error")
      }
    }
  }
  
  def sequenceOption[A](lst:List[Option[A]]):Option[List[A]] = {
    if(lst.contains(None))	None
//    else Some(lst.foldLeft(List[A]())((a,b) => a:+b.get))
//    else Some(for(i <- lst) yield i.get)
    else Some(lst.flatten)
  }
  
  def blub(vary:Int):Either[String, Int] = if(vary < 0) Left("Error: " + vary) else Right(vary)
  def blub2(varr:Int):Pair[Int, String] = if(varr >= 0) (varr, "Pos " + varr) else (varr, "Neg " + varr)
  
  import scala.annotation._
  @tailrec
  val exp:(Int,Int) => Either[BigDecimal,BigInt] = (i,n) => {
    if(n < 0) Left(BigDecimal(i).pow(n))
    else Right(BigInt(i).pow(n))
  }
}
	