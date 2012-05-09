package test

import scala.math.BigInt.int2bigInt


object Aufg4 {

object Environment{
 def printOS = {
   import java.util._
   val system = "# cores\t: "+  java.lang.Runtime.getRuntime().availableProcessors() +
   "\n# OS\t: "+System.getProperty("os.name")+" "+System.getProperty("os.version")+" "+System.getProperty("os.arch")+
   "\n# JVM \t: "+System.getProperty("java.version")
   println(system)
 }
}
  def fibo (n: Int):BigInt = ((0:BigInt, 1:BigInt) /: (1 to n)) ((a, x) => (a._2, a._1 + a._2))._1
  def main(args: Array[String]){

    
    var time = System.currentTimeMillis()
//    fibo(10000000)
//    println((System.currentTimeMillis()-time))
//    for(i <- 1 to 1000000) fibo(5)
//    println((System.currentTimeMillis()-time)/5)
//    time = System.currentTimeMillis()
//    for(i <- 1 to 1000000) fibo(10)
//    println((System.currentTimeMillis()-time)/10)
//    time = System.currentTimeMillis()
//    for(i <- 1 to 1000000) fibo(50)
//    println((System.currentTimeMillis()-time)/50)
//    time = System.currentTimeMillis()
//    for(i <- 1 to 1000000) fibo(100)
//    println((System.currentTimeMillis()-time)/100)
//    time = System.currentTimeMillis()
//    for(i <- 1 to 1000000) fibo(500)
//    println((System.currentTimeMillis()-time)/500)
//    time = System.currentTimeMillis()
//    for(i <- 1 to 1000000) fibo(1000)
//    println((System.currentTimeMillis()-time)/1000)
//    time = System.currentTimeMillis()
	Environment.printOS
	println
    println(Matrix2(3)(1,2,3))
    println(Matrix2(3)(1.0,2,3))
//    println(Matrix2(3)())
    println(Matrix2(3)(1))
    
    val mat1= Matrix2(5,(i,j) => if(i==j) 1 else 0)
    val mat2= Matrix2(Array(Array(3,2,1),Array(0,1,0),Array(0,0,3)))
    val mat3= Matrix2(1)(1.01)

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
    
    println(mat1 + mat1)
    println(Matrix2(3)(1,2,3) + mat2)
    println(Matrix2(3)(1,2,3) + mat2 + Matrix2(3,(i,j)=>i+j))
    println(mat3 + Matrix2(1)(0))
    
    println("@@@@@@@@@@@@")
    val mat4 = Matrix2(Array(Array(3,2,1,2),Array(0,1,0),Array(0,0,3)))
    println(mat4)
    println(mat3 + Matrix2(2)(0,0,0,0))
    println(Matrix2(2)(0.0) + mat3)
  }
}