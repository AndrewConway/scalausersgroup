package futures

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import org.greatcactus.xs.util._

/**
 * Demonstrate canceling an InterruptableFuture
 */

object DemoForComprehensionInterruptableFutures extends App {
  
  val started = System.currentTimeMillis();
  def log(s:String) = println("Time : %4d => %s".format(System.currentTimeMillis()-started,s))

  
  def expensive() : InterruptableFuture[Int]= InterruptableFuture.future {
    Thread.sleep(1000);
    // line below is not actually needed as Thread.sleep will do it automatically.
    if (Thread.interrupted()) throw new InterruptedException
    42
  }
  
  
  val f1 : InterruptableFuture[Int] = expensive() 
  val f2 : InterruptableFuture[Int] = expensive() 
  val f3 : InterruptableFuture[Int] = for (f1v<-f1;f2v<-f2) yield f1v+f2v
  
  // translates to 
  val f3a : InterruptableFuture[Int] = f1.flatMap{f1v=>for (f2v<-f2) yield f1v+f2v}
  // translates to 
  val f3b : InterruptableFuture[Int] = f1.flatMap{f1v=>f2.map{f2v => f1v+f2v}}
  
  f3.cancel()
  
  log("Future assigned to f");
  
  f1 onComplete {
    case Success(i) => log("Got f1 result "+i)
    case Failure(t) => log("f1 exception "+t.getMessage());
  }
  
  f2 onComplete {
    case Success(i) => log("Got f2 result "+i)
    case Failure(t) => log("f2 exception "+t.getMessage());
  }
  
  f3 onComplete {
    case Success(i) => log("Got f3 result "+i)
    case Failure(t) => log("f3 exception "+t.getMessage());
  }
  
  Thread.sleep(3000);
  log("Program terminating");
    
         // sensible next file to look at
  import futures.DemoForComprehensionObsoletableInterruptableFutures

}