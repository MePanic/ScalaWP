package test

import [T]test.Matrix2
import scala.Array.fallbackCanBuildFrom
import scala.math.Numeric
import scala.math.max

class Matrix2[T](val len:Int,val m:IndexedSeq[T]) {
  
//  def update(i:Int)(j:Int)(k:Int) = new Matrix2(len,  = k)
  
  def apply(i:Int,j:Int) = if(i<len && j < len) m(i*len + j) else Double.NaN

  override def toString = m.foldLeft(("Matrix(",0))((r,v) => 
		if(m.size == 1) (r._1 + m(0), 0)
	  	else if(r._2 == len-1) (r._1+","+v+")",0) 
		else if(r._2 == 0) (r._1+"("+v,r._2+1) 
		else (r._1+","+v,r._2+1))._1 + ")"
    
  def +(mat:Matrix2[T])(implicit num:Numeric[T]) = 
	    if (len != mat.len) new Matrix2(0,IndexedSeq[T]())
	    else new Matrix2(len, (m, mat.m).zipped map (num.plus(_,_)))
}

object Matrix2 {
    
//  def apply[T](len:Int)(implicit num:Numeric[T]) =
//    new Matrix2(len, IndexedSeq[T]().padTo(len*len, num.zero))
//  
//  def apply[T](len:Int)(elem:T)(implicit num:Numeric[T]) =
//    new Matrix2(len, IndexedSeq[T]().padTo(len*len, elem))
  
  def apply[T](len:Int)(lis:T*)(implicit num:Numeric[T]) = lis.length match {
    case 0 => new Matrix2(len, IndexedSeq[T]().padTo(len*len, num.zero))
    case 1 => new Matrix2(len, IndexedSeq[T]().padTo(len*len, lis.head))
    case _ => new Matrix2(len, lis.toIndexedSeq.padTo(len*len, num.zero))
  }
    
  def apply[T](a:Array[Array[T]])(implicit num:Numeric[T]) = {
    val len = ((a.size) /: (0 to a.size-1))((x,d) => max(x, a(d).size))
    new Matrix2(len , a.foldLeft(IndexedSeq[T]())(_++_.padTo(len, num.zero)))
  }
  
  def apply[T](size:Int,s:(Int,Int) => T) = 
   new Matrix2(size,for{i <- 0 until size; j <- 0 until size } yield s(i,j))
}