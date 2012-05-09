package test

import Matrizen.Matrix
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.Executors
import test.MatrixImplicit.num2Scalar

object MatrixImplicit{
  implicit def num2Scalar[N](l:N)(implicit num: Numeric[N]) = new Scalar(num.toDouble(l))
}

class Scalar(val value:Double){
  import Matrizen._
  
	def *(o:Matrix) = {
	  val ma = o.matrix.clone()
	  for{i1 <- 0 until o.matrix.length; i2 <- 0 until o.matrix(0).length} ma(i1)(i2) *= value
	  Matrix(ma)
	}
}


object Matrizen {
  
  class Matrix( a:Array[Array[Double]] ){
	val matrix = a
    
    override def toString = {    
      "Matrix(" + 
      matrix.foldLeft(""){ (accu,item) => accu+(item.deepMkString("(",",",")"))} + 
      ")"
    }
	
	override def equals(o:Any):Boolean = {
	  o match{
	    case a: Matrix => 
	    if(matrix.length == a.matrix.length){
	      for{i1 <- 0 until matrix.length; i2 <- 0 until matrix(0).length}{
	        if(matrix(i1)(i2) != a.matrix(i1)(i2)) return false
	      }
	      return true
	    }else{
	      return false
	    }
	    case _ => false   
	  }
	}
	
	def apply(i1:Int,i2:Int) = {
	  if(i1 >= 0 && i1 < a.length && a(0) != null){
	    if(i2 >= 0 && i2 < a(0).length ) matrix(i1)(i2)
	    else Double.NaN
	  }else{
	    Double.NaN
	  }	    
	}
	
	def +(o:Matrix) = {
	  val ma = Array.fill(matrix.length, matrix.length)(0.0)
	  if(matrix.length == o.matrix.length){	    
	    for{i1 <- 0 until matrix.length; i2 <- 0 until matrix(0).length}{
	      ma(i1)(i2) = matrix(i1)(i2) + o.matrix(i1)(i2)
	    }
	  }
	  new Matrix(ma)
	}
	
	def setNewValue(i1:Int, i2:Int, newVal:Double) = {
	  if(i1 >= 0 && i1 < matrix.length && matrix(0) != null){
	    if(i2 >= 0 && i2 < matrix.length ){
	      val n = Matrix(matrix.clone())// mit new Matrix(matrix.clone()) Referenzen!
	      n.matrix(i1)(i2) = newVal
	      n
	    }else{
	      this
	    }
	  }else{
	    this
	  }
	}
	
	def *(o:Matrix) = {
	  if(this.matrix.length == o.matrix.length){
	    val ma = Array.fill(matrix.length, matrix.length)(0.0)
	    for{i1 <- 0 until matrix.length; i2 <- 0 until matrix(0).length}{
	      ma(i1)(i2) = (0 until matrix.length).foldLeft(0.0){
	        (accu,item) => accu+matrix(i1)(item)*o.matrix(item)(i2)
	      }
	    }
	    new Matrix(ma)
	  }else{
	    this
	  }
	}
	
	def *(o:Double) = {
	  val ma = matrix.clone()
	  for{i1 <- 0 until matrix.length; i2 <- 0 until matrix(0).length} ma(i1)(i2) *= o
	  Matrix(ma)
	}
	
	def apply(i:Double*) = {
	  val ma = Array.fill(matrix.length, matrix.length)(0.0)
	  val iter = i.iterator
      if(i.length > 0 && i.length <= ma.length*ma.length ){
        for{i1 <- 0 until ma.length; i2 <- 0 until ma(0).length}{          
          if(iter.hasNext) ma(i1)(i2) = iter.next
          else ma(i1)(i2) = 0
        }
      }
	  new Matrix(ma)
    }  
	
	def length = {
	  matrix.length
	}
  }
  
  object Matrix{
    def apply(i:Int) = {
      new Matrix( Array.fill(i, i)(0.0) )
    }
    
    
    def apply[N](aArr:Array[Array[N]])(implicit num: Numeric[N]):Matrix = {     
	  val a = Array.fill(aArr.length, aArr.length)(0.0)
	  for{i1 <- 0 until aArr.length; i2 <- 0 until aArr.length}{ 
	    a(i1)(i2) = num.toDouble( aArr(i1)(i2) )
	  }
	  new Matrix(a)
    }
    
    
    def apply[N](n:Int, f: (Int,Int) => N)(implicit num:Numeric[N]) = {
      val ma = Array.fill(n, n)(0.0)
      for{i1 <- 0 until n; i2 <- 0 until n}{          
          ma(i1)(i2) = num.toDouble( f(i1,i2) )
      }
      new Matrix(ma)
    }
    
    def apply(blockq:ArrayBlockingQueue[(Int,Int,Double)], size:Int) = {
      val iter = blockq.iterator()         
      val resuArr = Array.fill(size, size)(0.0)
    
      while(iter.hasNext()){
    	val elem = iter.next()
    	resuArr(elem._1)(elem._2) = elem._3
      }
      new Matrix(resuArr)
    }
  }
  
  def main(s:Array[String]){
//    test
    test_multicore
  }
  
  object Environment {
      def p(x:String) = System.getProperty(x)
      def printOS = {
      println("# cores : " + getThreads)
      println("# OS    : " + p("os.name") +" "+ p("os.version") +" "+ p("os.arch"))
      println("# JVM   : " + p("java.version"))
      println
    }
      def getThreads = {
        Runtime.getRuntime().availableProcessors()
      }
  }
  
  def test_multicore{    
	Environment.printOS
    
    //Test mit 100x100 Matrix
    val mat1 = Matrix(100,(i,j) => 1)
    print("100x100, 1x  "); testbench(multiThread(mat1 ,mat1, 1), 100)
    print("100x100, 2x  "); testbench(multiThread(mat1, mat1, 2), 100)
    print("100x100, 3x  "); testbench(multiThread(mat1, mat1, 3), 100)
    println
    
    //Test mit 1000x1000 Matrix
    val mat2 = Matrix(1000,(i,j) => 1)
    print("1000x1000, 1x  "); testbench(multiThread(mat2, mat2, 1), 1)
    print("1000x1000, 2x  "); testbench(multiThread(mat2, mat2, 2), 1)
    print("1000x1000, 3x  "); testbench(multiThread(mat2, mat2, 3), 1)


    println("fertig")
  }
  
   def testbench(block: => Unit, loopCount:Int) = {
   var start = java.lang.System.currentTimeMillis()
   for(i <- 0 until loopCount)
     block
   println(java.lang.System.currentTimeMillis() - start+" ms")
 }
  
  def multiThread(matrix1:Matrix,matrix2:Matrix, multiply:Int) = {
  	val numOSThreads = Environment.getThreads
    val numThreads = numOSThreads*multiply
    val tasksPerThread = ( (matrix1.length*matrix1.length)/(numOSThreads*multiply*1.0D)+(0.999999)).asInstanceOf[Int]
    
    val barrier = new CyclicBarrier(numThreads+1)
    val blocking_queue = new ArrayBlockingQueue[(Int,Int,Double)](matrix1.length*matrix1.length)
    val tupels = for{i1 <- 0 until matrix1.length; i2 <- 0 until matrix1.length} yield (i1, i2)
//    println("tups: "+tups.toList.size)
    
    val executorPool = Executors.newFixedThreadPool(numThreads)
    for(i <- 0 until numThreads){
      executorPool.execute( funcToRunnable(tupels.toList.slice(i*tasksPerThread, tasksPerThread+i*tasksPerThread),matrix1,matrix2,barrier, blocking_queue) ) 
    }
    
    barrier.await()
//    print(" Einträge Queue: "+blocking_queue.size()+" ")
    Matrix(blocking_queue, matrix1.matrix.length)
  }
  

  def funcToRunnable( l:List[(Int,Int)], m1:Matrix, m2:Matrix, b:CyclicBarrier, q:BlockingQueue[(Int,Int,Double)]) = new Runnable(){ 
  	def run(){ 
  	  l.foreach( e => q.add(e._1, e._2, f(m1,m2,e._1, e._2)))
  	  b.await()
  	} 
  }
  
  def f(m1:Matrix, m2:Matrix, row:Int, col:Int) =  { (0 until m1.length).foldLeft(0.0){ (accu, item) => accu+m1.matrix(row)(item)*m2.matrix(item)(col)} }
  
  def test {
    import MatrixImplicit._
	Environment.printOS // man will ja wissen auf welcher Maschine man arbeitet
	println
	val ma1 = Matrix(2)(1,2,3,4)
	val ma2 = Matrix(2)(4,3,2,1)
	println(ma1*ma1)
	println(ma2*ma1)
	println(10*Matrix(2)(1,2,3,4) == Matrix(2)(1,2,3,4)*10)

//	println(op1*op2)
//	println(Matrix(3)(1.0,2,3))
//	val mat1= Matrix(5,(i,j) => if(i==j) 1 else 0) // 5x5 Einheits-Matrix
//	val mat2= Matrix(Array(Array(3,2,1),Array(0,1,0),Array(0,0,3)))
//	val mat3= Matrix(1)(1.01) // 1x1 Matrix
	
//	println("mat2: "+mat2)
//	println(mat3)
//	println(mat1(0,0))
//	println(mat1(4,0))
//	println(mat1(4,4))
//	println(mat1(5,5))
//	println(mat1 + mat1)
//	println(Matrix(3)(1,2,3) + mat2)
//	println(Matrix(3)(1,2,3) + mat2 + Matrix(3,(i,j)=>i+j))
//	println(mat3 + Matrix(1)(0))
//	
//	println("-------------")
//	val mat_4= Matrix(5,(i,j) => if(i==j) 1.0 else 0.0)
//	val mat_5 = Matrix(Array(Array(6.6,5.5,7.7),Array(6.6,5.5,7,7),Array(6.6,5.5,7,7)))
//	val mat_6 = Matrix( Array(Array(1.toChar,1.toChar),Array(2.toChar,2.toChar)))
//	val mat_7 = Matrix(Array(Array(1),Array(2)))
//	val mat_8 = Matrix(Array(Array(1,4,3),Array(2)))
//	println(mat_4)
//	println(mat_5)
//	println(mat_6)
//	println(mat_7)
//	println(mat_8)
  }

}