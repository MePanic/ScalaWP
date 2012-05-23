package test

object Test06 extends App {
  var curr = java.lang.System.currentTimeMillis
  (1 to 10) foreach (i => Thread.sleep(500))
  println(java.lang.System.currentTimeMillis - curr)
  
  curr = java.lang.System.currentTimeMillis
  (1 to 10).par foreach (i => Thread.sleep(500))
  println(java.lang.System.currentTimeMillis - curr)

  curr = java.lang.System.currentTimeMillis
  (1 to 10).par foreach (i => Thread.sleep(500))
  println(java.lang.System.currentTimeMillis - curr)

  curr = java.lang.System.currentTimeMillis
  (1 to 10).par foreach (i => Thread.sleep(500))
  println(java.lang.System.currentTimeMillis - curr)
}