//package test
//
//import scala.Math._
//
//object MatrixNew {
//	def apply[T](size:Int)(vargs:T*)(implicit num:Numeric[T]) = new ValidMatrix(size,List()++vargs)
//	
//}
//
//trait MatrixNew[T] {	
//	def apply(t:T)
//}
//
//class ValidMatrix[T](val size:Int,val m:List[T]) extends MatrixNew[T]{
//	override def toString:String = "Matrix(" + size + "," + m + ")"
//	
//	def apply(int:T) = {
//	  println(int,size,m)
//	  
//	}
//}
//
//object NaM extends MatrixNew[Double]{
//	override def toString:String = "NaM"
//	def apply(int:Double){
//	  NaM
//	  
//	}
//  
//}
//
