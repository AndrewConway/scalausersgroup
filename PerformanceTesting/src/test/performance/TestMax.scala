/**
 * Copyright 2012 Silicon Econometrics Pty. Ltd. All rights reserved
 */
package test.performance

/**
 * Note - gcc -O3 -funroll-loops
 */
abstract class TestMax(val name:String) {
  def numTests = 1000
  def max(values:Array[Int]) : Int
}

object TestMax {
  def test(t:TestMax,values:Array[Int]) {
    t.max(values) // make sure classes are loaded.
    System.gc()
    val startTime = System.currentTimeMillis
    var results : Set[Int] = Set.empty
    for (i<-0 until t.numTests) {
      val res = t.max(values)
      if (!results.contains(res)) results+=res
    }
    val endTime = System.currentTimeMillis
    val timePerIteration = (endTime-startTime).doubleValue/t.numTests
    println("%s\t%s\t%.3fms".format(t.name,results.mkString(","),timePerIteration))
  }
  
  val tests = List(SimpleMax, // ParallelMax,
      LoopTest,LoopWithIfTest,
      //LoopTestSepFN,LoopWithIfTestSepFN, LoopParTest,LoopParTest2,
      //LoopRangeTest,LoopRangeTestIf,LoopRangeTestIf2,
      //ExplicitLoopTestStatic,ExplicitLoopTestStatic,
      ExplicitLoopTest,ExplicitLoopIfTest,ExplicitLoopIndexedSeq,ExplicitLoopIndexedSeq2,ExplicitUnrolledLoopTest,
      new ExplicitLoopParallelizedWithFutures(1),new ExplicitLoopParallelizedWithFutures(2),new ExplicitLoopParallelizedWithFutures(4),new ExplicitLoopParallelizedWithFutures(8),
      new ExplicitLoopParallelizedWithCollections(1),new ExplicitLoopParallelizedWithCollections(2),new ExplicitLoopParallelizedWithCollections(4),new ExplicitLoopParallelizedWithCollections(8),
      new ExplicitLoopParallelizedWithCollections2(1),new ExplicitLoopParallelizedWithCollections2(2),new ExplicitLoopParallelizedWithCollections2(4),new ExplicitLoopParallelizedWithCollections2(8) // ,
      //FoldTest,ReduceTest ,FoldParTest,ReduceParTest
      )
  
  def main(args:Array[String]) {
    val size = 10000000
    println(Runtime.getRuntime.maxMemory/1024.0/1024.0)
    val values = (for (i<-0 until size) yield (Math.random*10000000).toInt).toArray
    for (t<-tests) test(t,values)
  }
}

object SimpleMax extends TestMax("simple") {
  override def max(values:Array[Int]) = values.max
}

object ParallelMax extends TestMax("parallel simple") {
  override def numTests = super.numTests/20
  override def max(values:Array[Int]) = values.par.max
}


object LoopTest extends TestMax("loop") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    for (i<-values) res = res max i
    res
  }
}

object LoopWithIfTest extends TestMax("loop with if") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    for (i<-values) if (res<i) res = i
    res
  }
}

object LoopTestSepFN extends TestMax("loop separate function") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    def f(i:Int) { res=res max i }
    for (i<-values) f(i)
    res
  }
}

object LoopWithIfTestSepFN extends TestMax("loop with if separate function") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    def f(i:Int) {if (res<i) res = i}
    for (i<-values) f(i)
    res
  }
}


object LoopParTest extends TestMax("naive parallel loop") {
  override def numTests = super.numTests/20
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    for (i<-values.par) res = res max i
    res
  }
}

object LoopParTest2 extends TestMax("synchronized parallel loop") {
  override def numTests = super.numTests/20
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    for (i<-values.par) synchronized { res = res max i }
    res
  }
}

object LoopRangeTest extends TestMax("loop range") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    for (i<-0 until values.length) 
       res = res max values(i)
    res
  }
}

object LoopRangeTestIf extends TestMax("loop range with if") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    for (i<-0 until values.length) {
       val v = values(i)
       if (v>res) res=v
    }
    res
  }
}

object LoopRangeTestIf2 extends TestMax("loop range with if no local vars") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    for (i<-0 until values.length) {
       if (values(i)>res) res=values(i)
    }
    res
  }
}



object ExplicitLoopTest extends TestMax("explicit loop") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    var count = 0
    val endLoop = values.length
    while (count<endLoop) {
      val i = values(count)
      res = res max i
      count+=1
    }
    res
  }
}


object ExplicitLoopTestStatic extends TestMax("explicit loop Math.max") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    var count = 0
    val endLoop = values.length
    while (count<endLoop) {
      val i = values(count)
      res = java.lang.Math.max(res,i)
      count+=1
    }
    res
  }
}

object ExplicitUnrolledLoopTest extends TestMax("explicit unrolled loop with if") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    var count = 0
    val endLoop = values.length
    while (count+9<endLoop) {
      { val i = values(count); if (i>res) res=i }
      { val i = values(count+1); if (i>res) res=i }
      { val i = values(count+2); if (i>res) res=i }
      { val i = values(count+3); if (i>res) res=i }
      { val i = values(count+4); if (i>res) res=i }
      { val i = values(count+5); if (i>res) res=i }
      { val i = values(count+6); if (i>res) res=i }
      { val i = values(count+7); if (i>res) res=i }
      { val i = values(count+8); if (i>res) res=i }
      { val i = values(count+9); if (i>res) res=i }
      count+=10
    }
    while (count<endLoop) {
      val i = values(count)
      if (i>res) res=i 
      count+=1
    }
    res
  }
}


object ExplicitLoopIfTest extends TestMax("explicit loop with if") {
  override def max(values:Array[Int]) = {
    var res = Integer.MIN_VALUE
    var count = 0
    val endLoop = values.length
    while (count<endLoop) {
      val i = values(count)
      if (i>res) res=i 
      count+=1
    }
    res
  }
}

class Parallelize(threads:Int,values:Array[Int]) {
  /** Compute a maximum from from to to */
  def subset(from:Int,to:Int) = {
       val localvalues = values
       var res = Integer.MIN_VALUE
       var count = from
       while (count<to) {
          val i = localvalues(count)
          if (i>res) res=i 
          count+=1
       }
       res
  }
  def pos(i:Int) = (values.length*i)/threads
  def doSubset(i:Int) = subset(pos(i),pos(i+1))
}

class Parallelize2(numThreads:Int,values:Array[Int]) {
  /** max of every numThreads'th element starting from start */
  def subset(start:Int) = {
       val localvalues = values
       val end = values.length
       val inc = numThreads
       var res = Integer.MIN_VALUE
       var count = start
       while (count<end) {
          val i = localvalues(count)
          if (i>res) res=i 
          count+=inc
       }
       res
  }
}



class ExplicitLoopParallelizedWithFutures(threads:Int) extends TestMax("explicit loop with if parallelized futures ("+threads+")") {
  override def max(values:Array[Int]) = {
    val work = new Parallelize(threads,values)
    val futures = for (i<-0 until threads) yield scala.actors.Futures.future {work.doSubset(i)}
    futures.map{_()}.max
  }
}

class ExplicitLoopParallelizedWithCollections(threads:Int) extends TestMax("explicit loop with if parallelized collection ("+threads+")") {
  override def max(values:Array[Int]) = {
    val work = new Parallelize(threads,values)
    (0 until threads).par.map{work.doSubset(_)}.seq.max
  }
}

class ExplicitLoopParallelizedWithCollections2(threads:Int) extends TestMax("explicit loop with if parallelized collection 2 ("+threads+")") {
  override def max(values:Array[Int]) = {
    val work = new Parallelize2(threads,values)
    (0 until threads).par.map{work.subset(_)}.seq.max
  }
}



object ExplicitLoopIndexedSeq extends TestMax("explicit loop with if and IndexedSeq") {
  override def max(values:Array[Int]) = {
    val is : IndexedSeq[Int] = values
    var res = Integer.MIN_VALUE
    var count = 0
    val endLoop = values.length
    while (count<endLoop) {
      val i = is(count)
      if (i>res) res=i 
      count+=1
    }
    res
  }
}

object ExplicitLoopIndexedSeq2 extends TestMax("explicit loop with if and IndexedSeq created twice") {
  override def max(values:Array[Int]) = {
    val is : IndexedSeq[Int] = values
    val is2 : IndexedSeq[Int] = values
    var res = is2.head
    var count = 0
    val endLoop = values.length
    while (count<endLoop) {
      val i = is(count)
      if (i>res) res=i 
      count+=1
    }
    res
  }
}


object FoldTest extends TestMax("fold") {
  override def max(values:Array[Int]) = {
    values.fold(Integer.MIN_VALUE)(_ max _)
  }
}

object ReduceTest extends TestMax("reduce") {
  override def max(values:Array[Int]) = {
    values.reduce(_ max _)
  }
}

object FoldParTest extends TestMax("fold parallel") {
  override def numTests = super.numTests/20
  override def max(values:Array[Int]) = {
    values.par.fold(Integer.MIN_VALUE)(_ max _)
  }
}

object ReduceParTest extends TestMax("reduce parallel") {
  override def numTests = super.numTests/20
  override def max(values:Array[Int]) = {
    values.par.reduce(_ max _)
  }
}

object TestThreads {
  def workTest() { for (i<-1 to 500000000) i*i; println("@") }
  def fwork() = scala.actors.Futures.future {workTest()}
   def onesec () { Thread.sleep(1000); println("@")}
   def oneSecWork = scala.actors.Futures.future {onesec()}
}

object TestPar1 {
  def main(args:Array[String]) {
    val w = TestThreads.fwork
    w()
  }
}

object TestPar2 {
  def main(args:Array[String]) {
    val w1 = TestThreads.fwork
    val w2 = TestThreads.fwork
    w1()
    w2()
  }
}



/**
 * Results for 10 000 000 on Windows, 4/8 processor, JVM7
 * 
 * 
 * simple : 9999998 in 58.500ms
loop : 9999998 in 43.210ms
loop with if : 9999998 in 79.100ms
loop separate function : 9999998 in 96.250ms
loop with if separate function : 9999998 in 95.940ms
naive parallel loop : 9999991,9999998,9999995,9999989,9999982,9999993,9999996,9999997 in 656.760ms
synchronized parallel loop : 9999998 in 1906.400ms
explicit loop : 9999998 in 11.240ms
explicit loop with if : 9999998 in 5.630ms
explicit loop with if and IndexedSeq : 9999998 in 37.130ms
explicit loop with if parallelized futures (1) : 9999998 in 6.080ms
explicit loop with if parallelized futures (2) : 9999998 in 3.590ms
explicit loop with if parallelized futures (4) : 9999998 in 3.290ms
explicit loop with if parallelized futures (8) : 9999998 in 2.660ms
explicit loop with if parallelized collection (1) : 9999998 in 5.930ms
explicit loop with if parallelized collection (2) : 9999998 in 3.760ms
explicit loop with if parallelized collection (4) : 9999998 in 2.810ms
explicit loop with if parallelized collection (8) : 9999998 in 2.800ms
fold : 9999998 in 110.300ms
reduce : 9999998 in 126.520ms
fold parallel : 9999998 in 1302.150ms
reduce parallel : 9999998 in 1244.570ms


Results on Linux, 2 processor, (build 14.0-b16, mixed mode)
simple : 9999999 in 89.280ms
loop : 9999999 in 168.880ms
loop with if : 9999999 in 113.090ms
loop separate function : 9999999 in 185.320ms
loop with if separate function : 9999999 in 136.140ms
naive parallel loop : 9999999 in 2563.620ms
synchronized parallel loop : 9999999 in 2792.400ms
explicit loop : 9999999 in 96.240ms
explicit loop with if : 9999999 in 7.750ms
explicit loop with if and IndexedSeq : 9999999 in 75.900ms
explicit loop with if parallelized futures (1) : 9999999 in 8.510ms
explicit loop with if parallelized futures (2) : 9999999 in 8.210ms
explicit loop with if parallelized futures (4) : 9999999 in 8.120ms
explicit loop with if parallelized futures (8) : 9999999 in 8.670ms
explicit loop with if parallelized collection (1) : 9999999 in 7.980ms
explicit loop with if parallelized collection (2) : 9999999 in 8.110ms
explicit loop with if parallelized collection (4) : 9999999 in 8.300ms
explicit loop with if parallelized collection (8) : 9999999 in 8.350ms
fold : 9999999 in 234.700ms
reduce : 9999999 in 227.630ms
fold parallel : 9999999 in 2890.150ms
reduce parallel : 9999999 in 2848.320ms

C:

./a.out
Found 999999 in 38.377ms
-bash-4.1$ gcc -O3 maxtest.c
-bash-4.1$ ./a.out
Found 999999 in 13.669ms
-bash-4.1$ vi maxtest.c



 */





