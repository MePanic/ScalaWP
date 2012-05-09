package test

import scala.math._
import java.util.concurrent._
  
  class Matrix[T](val len:Int,val m:Array[Array[T]] ){
	
  def apply(i:Int,j:Int) = if(i<len && j < len) m(i)(j) else Double.NaN

  override def toString = {   
      "Matrix(" +
      m.foldLeft(""){ (accu,item) => accu+(item.mkString("(",",",")"))} +
      ")"}
	
	
	def +(mat:Matrix[T])(implicit num:Numeric[T]) = {    
		val res = m.clone()
	  if(len == mat.len){	
	    for{i1 <- 0 until len
	    	i2 <- 0 until len} res(i1)(i2) = num.plus(m(i1)(i2),mat.m(i1)(i2))
	  }
	  new Matrix(len,res)
	}
	
	def set(i1:Int, i2:Int, to:T) = {
	  if(i1 >= 0 && i1 < len &&	i2 >= 0 && i2 < len ){
	      val res = new Matrix(len,m.clone())
	      res.m(i1)(i2) = to
	      res
	    }else this
	}
	
	def *(mat:Matrix[T])(implicit num:Numeric[T]) = {
	  if(len == mat.len){	    
		val res = new Matrix(len,m.clone())
	    for{i1 <- 0 until len
	    	i2 <- 0 until len}
	    	res.m(i1)(i2) = (0 until len).foldLeft(num.zero){(accu,t) => num.plus(accu,num.times(m(i1)(t),mat.m(t)(i2)))}
	    new Matrix(len,res.m)
	  }else this
	}
	
	def *(value:T)(implicit num:Numeric[T]) = {
	  val res = new Matrix(len,m.clone())
	  for{i1 <- 0 until len
		  i2 <- 0 until len} res.m(i1)(i2) = num.times(res.m(i1)(i2),value)
	  res
	}
  }
  
  object Matrix{
  
//    def apply(len:Int) = new Matrix(len,Array.fill(len,len)(0.0))
    
//    def apply[T](len:Int,v:T = 0.0)(implicit num: Numeric[T], cm: ClassManifest[T]) = {
//      new Matrix(len,Array.fill(len,len)(v))
//    }
    
    
  def apply[T](len:Int)(lis:T*)(implicit num:Numeric[T], cm: ClassManifest[T]):Matrix[T] = {
	  if(lis.length == 0)return new Matrix(len, Array.fill(len,len)(num.zero))
	  if(lis.length == 1)return new Matrix(len, Array.fill(len,len)(lis(0)))
	  
      val res = Array.fill(len,len)(num.zero)
      for(i <- 0 until lis.length) res(i/len)(i%len) = lis(i)
      return new Matrix(len, res)
    }
    
  def apply[T](a:Array[Array[T]])(implicit num:Numeric[T], cm: ClassManifest[T]) = {
    val len = max(a.maxBy(_.size).size,a.size)
    val res = Array.fill(len,len)(num.zero)
    for{i <- a.indices
        j <- a(i).indices}res(i)(j) = a(i)(j)
    new Matrix(len , res)
  }
  
  def apply[T](len:Int,s:(Int,Int) => T)(implicit num:Numeric[T], cm: ClassManifest[T]) = {
    val res = Array.fill(len,len)(num.zero)
	   for{i <- res.indices
		   j <- res(i).indices } res(i)(j) =  s(i,j)
    new Matrix(len,res)
  }
  
  private def apply[T](len:Int, trip:ArrayBlockingQueue[(Int,Int,T)])(implicit num:Numeric[T], cm: ClassManifest[T]) = {
    val res = Array.fill(len,len)(num.zero)
    val iter = trip.iterator()
    while(iter.hasNext()){
    	val i = iter.next()
    	res(i._1)(i._2) = i._3
    }
    new Matrix(len, res)
  }
  //----------------------------------------------------------------------------
  
  def testbench(block: => Unit, loopCount:Int) = {
   var start = java.lang.System.currentTimeMillis()
   for(i <- 0 until loopCount)
     block
   println(java.lang.System.currentTimeMillis() - start+" ms")
 }
  
  def multiThread[T](m1:Matrix[Double],m2:Matrix[Double], times:Int) = {
  	val numCores = Environment.getCores
    val numThreads = numCores*times
    val tasksPerThread = ( (m1.len)/(numCores*times*1.0D))
    val cBarr = new CyclicBarrier(numThreads+1)
    val blockingQueue = new ArrayBlockingQueue[(Int,Int,Double)](m1.len*m1.len)
    
    val executorPool = Executors.newFixedThreadPool(numThreads)
    for(i <- 0 until numThreads){
      executorPool.execute( funcToRunnable(((i*tasksPerThread).toInt until (tasksPerThread+i*tasksPerThread).toInt),m1,m2,cBarr, blockingQueue) ) 
    }
    
    cBarr.await()
    Matrix(m1.len, blockingQueue)
  }
  

  def funcToRunnable[T]( l:Range, m1:Matrix[Double], m2:Matrix[Double], cBarr:CyclicBarrier, queue:BlockingQueue[(Int,Int,Double)]) = new Runnable(){ 
  	def run(){ 
  	  for(i <- 0 to m1.m.size-1;j <- l) queue.add(i,j,f(i,j,m1,m2))
  	  cBarr.await()
  	} 
  }
  
  def f(row:Int, col:Int,m1:Matrix[Double], m2:Matrix[Double] ) =  
  { (0 until m1.len).foldLeft(0.0){ (accu,item) => accu+m1.m(row)(item)*m2.m(item)(col)} }
    
  
  //----------------------------------------------------------------------------
  def main(s:Array[String]){
    test
    test_multicore
  }

  object Environment {
      def p(x:String) = System.getProperty(x)
      def printOS = {
      println("# cores : " + getCores)
      println("# OS    : " + p("os.name") +" "+ p("os.version") +" "+ p("os.arch"))
      println("# JVM   : " + p("java.version"))
      println
    }
      def getCores = {
        Runtime.getRuntime().availableProcessors()
      }
  }

  
  def test {
	Environment.printOS
	println
    println(Matrix(3)(1,2,3))
    println(Matrix(3)(1.0,2,3))
    
    val mat1:Matrix[Int]= Matrix(5,(i,j) => if(i==j) 1 else 0)
    val mat2= Matrix(Array(Array(3,2,1),Array(0,1,0),Array(0,0,3)))
    val mat3= Matrix(1)(1.01)

    println(mat1)
    println("------")
    println(mat2)
    println("------")
    println(mat3)
    println("---")
    println(mat1(0,0))
    println(mat1(4,0))
    println(mat1(4,4))
    println(mat1(5,5))
    println(Matrix(4)(1))
    
    println(mat1 + mat1)
    println(Matrix(3)(1,2,3) + mat2)
    println(Matrix(3)(1,2,3) + mat2 + Matrix(3,(i,j)=>i+j))
    println(mat3 + Matrix(1)(0))
    
    println("@@@@@@@@@@@@")
    val mat4 = Matrix(Array(Array(3,2,1,2),Array(0,1,0),Array(0,0,3)))
    println(mat4)
    println(mat3 + Matrix(2)(0,0,0,0))
    println(Matrix(2)(0.0) + mat3)
  }

    def test_multicore{    
    Environment.printOS
      
    //Test mit 100x100 Matrix
    val mat1:Matrix[Double]= Matrix(100,(i,j) => 1)
    print("100x100, 1x  "); testbench(multiThread(mat1 ,mat1, 1), 100)
    print("100x100, 2x  "); testbench(multiThread(mat1, mat1, 2), 100)
    print("100x100, 3x  "); testbench(multiThread(mat1, mat1, 3), 100)
    println
    
    //Test mit 1000x1000 Matrix
    val mat2:Matrix[Double] = Matrix(1000,(i,j) => 1)
    print("1000x1000, 1x  "); testbench(multiThread(mat2, mat2, 1), 1)
    print("1000x1000, 2x  "); testbench(multiThread(mat2, mat2, 2), 1)
    print("1000x1000, 3x  "); testbench(multiThread(mat2, mat2, 3), 1)


    println("fertig")
  }
}
  
  