package test

object Permutations {

  trait Permutation {
    def apply(i:Int):Int
    def cycles:IndexedSeq[List[Int]]
    def ==(arg:Permutation):Boolean
    def fixPoints:List[Int]
  }
  
  class ValidPerm(args:Collection[Int]) extends Permutation {
    
    val perm = args.toList
    
    val cycle = cycleNot(perm)
    
    def apply(i:Int) = perm(i-1)
    
    override def toString = {args.foldLeft("s(")((r,c) => r+c+",").init+")"}
    
    private def cycleNot(args:List[Int]) = {
      val v = scala.collection.mutable.Set[Int]()
      
      def gCyc(p:Int, l:List[Int]):List[Int] = if(args(l.head-1) == p) l.reverse
      								 else {v+=args(l.head-1);gCyc(p,args(l.head-1)::l)}
      
      for(i <- 1 to args.size if !v(i)) yield gCyc(i, i::Nil)
    }
    
    def cycles = cycle
    
    def ==(arg:Permutation) = this.toString() == arg.toString()
    
    def fixPoints = cycle.foldLeft(List[Int]())((r,c) => if(c.size == 1){ r:+c(0)} else {r})
  }
  
  object NaP extends Permutation{
    override def toString = "NaP"
    def apply(i:Int) = -1
    def cycles = Vector(List())
    def ==(arg:Permutation) = false
    def fixPoints = List()
  }
  
  def checkValidity(args:Collection[Int]) = args.toList.sorted.apply(args.toList.sorted.size-1) == args.toList.sorted.size && args.toList.sorted.size == args.toList.sorted.toSet.size  
  
  def s(args:Int*):Permutation = {
    if(!checkValidity(args)){ NaP} 
    else {new ValidPerm(args)}    
  }
  
}