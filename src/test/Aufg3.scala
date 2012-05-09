package test

import Permutations.NaP
import Permutations.s

object Aufg3 {
  
	def main(args: Array[String]){	  
	  	println("----------")
		val rs = RString("hello there")
		println(rs(1))
		println(rs(1,3))
		println(rs(1 to 3))
		println(rs(-4 to -2))
		println(rs(-3,2))
		println(rs(-3,4))
		println(rs("bye"))
		println("---------------")
		println("---------------")
		val sx1 = s(1,2,3,4)
		val sx2 = s(2,1,4,3)
		val sx3 = s(1,2,5,4)
		val sx4 = s(2,1,4,3)
		val sx5 = s(2,1,3,5,4)
		println(sx1)
		println(sx2)
		println(sx3)
		println(sx1(3))
		println(sx3(3))
		println(sx1==sx3)
		println(sx2==sx4)
		println(sx4 cycles)
		println(sx5 fixPoints)
		println("---------------")
		println("---------------")
		val s1= s(2,4,5,1,3)
		val s2= s(2,4,5,1,3)
		val s3= s(2,4,1,5,3)
		println(s1)
		println(s1==s2)
		println(s1==s3)
		println(s1(3))
		println(s3(3))
		println(s1 cycles)
		println(s3 cycles)
		println(s(1,2,3,4,5) fixPoints)
		println("---------------")
		val err1= s(2,5,8,5,4,6,3)
		val err2= s(2,5,8,5,4,6,3,1)
		println(err1)
		println(err2)
		println(NaP cycles)
		println(NaP fixPoints)
		println(NaP(10))
		println("---------------")

//		def foo (f: Int => Int, g: Int => Double, x: Int) = g(f(x))
//		println(foo(s(1,6,4,2,3,5), Array(1.0,-1.0,1,4.0,-3.0), 4))

		
	}
  
	class RS(arg:String){
	  
	  def apply(i:Int) = if(i <= arg.size){arg(i)} else { new Rnil }
	  
	  def apply(i:Int, r:Int) = {
	    if(r<0){ ""
	    } else if(i+r > arg.size){new Rnil
	    } else if(i >= 0){
	      arg.slice(i, i+r)	      
	    } else if((i + r) <= 0) {
	      arg.slice(arg.size+i, arg.size+i+r)	      
	    } else { new Rnil }
	  }
	  
	  def apply(r:Range) = {
	    if(r.size == 0) {""}
	    else if(r.size > arg.size||
	    		(r(0) < 0 && r(r.size-1) > 0)||
	    		(r(0)+r(r.size-1) > arg.size)){new Rnil}
	    else if(r(0) >= 0){arg.slice(r(0), r(r.size-1)+1)}
	    else {arg.slice(arg.size+r(0), arg.size+r(r.size-1)+1)}
	    
	  }
	  
	  def apply(x:Any) = new Rnil

	}
	  class Rnil(){
	    override def toString = "Rnil"
	  }
	  def RString(arg:String) = new RS(arg)
	}