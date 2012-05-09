package test

object PRP2Test {
  
  	def transposeList(list:List[List[String]]) =
	  for(i <- 0 to list(1).size-1) yield for(j <- 0 to list.size-1) yield list(j)(i)

	def symmDiff(s1:Set[String], s2:Set[String]) = (s1--s2)++(s2--s1)
	  
    def deepCopyTree[T](s:Any):Any = s match {
      case s:Set[T] => (Set[Any]() /: s)((a,b) => a+deepCopyTree(b))
      case _ => s}
  	
  	def setToMultiMap(s:Set[List[String]]) = 
  	  s.foldLeft(Map[String, List[String]]())((a,b) => 
  	    a+(b(0) -> (for(i <- s if(b(0) == i(0))) yield i(1)).toList))
  	 
	def main(args:Array[String]){
	  val list1 = List(List("a","b","c"),List("x","y","z"))
	  val set1 = Set("a","b")
	  val set2 = Set("b","c")
	  val set3 = Set("a",Set(Set("b","c"),"d"))
	  val set4 = Set(List("Emil","PR1"),List("Otto","PR2"),List("Emil","AF"))
	   println(transposeList(list1))
	   println(symmDiff(set1, set2))
	   println(deepCopyTree(set3))
	   println(setToMultiMap(set4))
	}
}