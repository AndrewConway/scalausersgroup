/**
 * Copyright 2012 Silicon Econometrics Pty. Ltd. All rights reserved
 */
package test.performance

/**
 * Test the performance overhead of value classes pre/post 2.10
 */

object TestValueClasses {
  def test(name:String,work : () => Unit) {
    work()  // make sure classes are loaded.
    System.gc()
    val startTime = System.nanoTime
    work()
    val endTime = System.nanoTime
    val timePerIteration = (endTime-startTime).doubleValue/1000000.0
    println("%s\t%.3fms".format(name,timePerIteration))
  }
  

  val size = 100000
  val repetitions = 1000
  def workPlain() {
    val values = new Array[Double](size)
    val inds = (0 until size).toArray
    val magic = 87
    for (dummy<-0 until repetitions) {
      for (i<-0 until size) {
        val ind = inds(i)
        values(ind)+= (if (ind==magic) 1.0 else 2.0)
      }
    }
  }
  
  def workIndex() {
    val values = new Array[Double](size)
    val inds = (0 until size).toArray.map{CompiledVariableIndex(_)}
    val magic = CompiledVariableIndex(87)
    for (dummy<-0 until repetitions) {
      for (i<-0 until size) {
        val ind = inds(i)
        values(ind.index)+= (if (ind==magic) 1.0 else 2.0)
      }
    }
  }
  
  def workFull() {
    val values = new CompiledVariableValues(new Array[Double](size))
    val inds = (0 until size).toArray.map{CompiledVariableIndex(_)}
    val magic = CompiledVariableIndex(87)
    for (dummy<-0 until repetitions) {
      for (i<-0 until size) {
        val ind = inds(i)
        values(ind)+= (if (ind==magic) 1.0 else 2.0)
      }
    }
  }
  
  def main(args:Array[String]) {
    val size = 10000000
    println(Runtime.getRuntime.maxMemory/1024.0/1024.0)
    test("Plain",workPlain _)
    test("Index",workIndex _)
    test("Full",workFull _)
  }
}


/** Compiled variables are represented as integer offsets into an array. For type safety it can be useful to wrap them in this class to stop them being confused with other integers. */
class CompiledVariableIndex(val index:Int) { // TODO extends AnyVal in 2.10 and get rid of equals and hashcode.
  override def equals(other: Any) = other match {
       case that: CompiledVariableIndex => this.index == that.index
       case _ => false
  }

  override def hashCode = index
  override def toString = index.toString
}

object CompiledVariableIndex {
  def apply(index:Int) = new CompiledVariableIndex(index)
} 

/** The value of Compiled variables are represented as an array of doubles, indexed by CompiledVariableIndex. For type safety it can be useful to wrap them in this class to stop them being confused with other integers. */
class CompiledVariableValues(val data:Array[Double]) { // TODO make a value class in 2.10
  def apply(index:CompiledVariableIndex) = data(index.index)
  def update(index:CompiledVariableIndex,value:Double) { data(index.index)=value }
}

