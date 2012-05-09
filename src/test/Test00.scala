package test

object Test01{
	
  def sort[T <% Ordered[T]](arg1:T, args:T*) = {
    (arg1 +: args).sortWith(_<_)
    }
  
  case class PointI(x:Int, y:Int) extends Ordered[PointI]{
    def compare(that:PointI):Int = {
      if(x.compareTo(that.x) == 0){y.compareTo(that.y)}
      else x.compareTo(that.x)
    }
    
  }
  
  case class PointD(x:Double, y:Double) extends Ordered[PointD]{
    def compare(that:PointD):Int = {
      if(x.compareTo(that.x) == 0){y.compareTo(that.y)}
      else x.compareTo(that.x)
    }
    
  }
  
  def test01{
    println(sort(1,2,-1,0,2,2,3))
    println(sort(PointD(1,2),PointD(1,3),PointD(1,2),PointD(0,10),PointD(4,-10)))
  }
	
  def main(args: Array[String]){
    test01
  }
}