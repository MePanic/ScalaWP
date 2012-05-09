package test

import java.io.IOException
import scala.Array.canBuildFrom
import scala.io.Source.fromFile
import scala.util.control.Exception.catching

object Test04 {

  def fromTextFile(path:String):Either[Throwable, Array[Byte]]/*:Option[Array[Byte]]*/ = {
//	try {
//    val source = fromFile(path)
//    
//    val lines = source.getLines().drop(1).mkString
//    source.close() 
//    Some(lines.filter(a => a=='0' || a=='1').foldLeft(Array[Byte]())((a,b) => a:+b.toByte))
//	} catch {
//	  case _ => println("Error. Something went wrong...")
//	  None
//	}
    
//    handling(classOf[IOException]) by (_ => None) apply {
//    val source = fromFile(path)
//    val lines = source.getLines().drop(1).mkString
//    source.close() 
//    Some(lines.filter(a => a=='0' || a=='1').foldLeft(Array[Byte]())((a,b) => a:+b.toByte)) 
//    }
    
    catching(classOf[IOException]) either {
    val source = fromFile(path)
    val lines = source.getLines().drop(1).mkString
    source.close() 
    lines.filter(a => a=='0' || a=='1').foldLeft(Array[Byte]())((a,b) => a:+b.toByte) 
    }
  }
  
  def myAppend[T](l1:List[T], l2:List[T]):List[T] = l2.foldLeft(l1)((a,b) => a:+b)
  
  def myConcat[T](x:List[List[T]]):List[T] = x.foldLeft(List[T]())((a,b) => myAppend(a,b))
  
  def myConcatMap[A,B](x: List[List[A]], f: List[A] => List[B]):List[B] = f(myConcat(x))
      
  def func(s:List[String]):List[Int] = s.foldLeft(List[Int]())((a,b) => a:+b.length())
  
  def main(args:Array[String]){
    println(fromTextFile("/home/meph/test.txt").fold((a) => a,_ deep))
    println(myConcat(List(List(1,2),List(3,4,5))))
    println(myConcatMap(List(List("a","abc"),List("defg")), func))
  }
  
}