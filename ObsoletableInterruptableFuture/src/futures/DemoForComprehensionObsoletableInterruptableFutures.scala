package futures

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import org.greatcactus.xs.util._

/**
 * Demonstrate obsolescence in an ObsoletableAndInterruptableFuture.
 */
object DemoForComprehensionObsoletableInterruptableFutures extends App {
  
  val started = System.currentTimeMillis();
  def log(s:String) = println("Time : %4d => %s".format(System.currentTimeMillis()-started,s))

  
  def expensive() : InterruptableFuture[Int]= InterruptableFuture.future {
    Thread.sleep(1000);
    // line below is not actually needed as Thread.sleep will do it automatically.
    if (Thread.interrupted()) throw new InterruptedException
    42
  }
  
  object f1Obsoleter extends ChangeHandle {
    def dispose() { log("No longer care about f1 obsolescence")}
  }
  object f2Obsoleter extends ChangeHandle {
    def dispose() { log("No longer care about f2 obsolescence")}
    future { // In two seconds, make this value obsolete.
       Thread.sleep(2000)
       change()
    }
  }
  
  
  val f1 : ObsoletableAndInterruptableFuture[Int] = new ObsoletableAndInterruptableFuture(expensive(),List(f1Obsoleter)) 
  val f2 : ObsoletableAndInterruptableFuture[Int] = new ObsoletableAndInterruptableFuture(expensive(),List(f2Obsoleter)) 
  val f3 : ObsoletableAndInterruptableFuture[Int] = for (f1v<-f1;f2v<-f2) yield f1v+f2v
  
  // translates to 
  val f3a : ObsoletableAndInterruptableFuture[Int] = f1.flatMap{f1v=>for (f2v<-f2) yield f1v+f2v}
  // translates to 
  val f3b : ObsoletableAndInterruptableFuture[Int] = f1.flatMap{f1v=>f2.map{f2v => f1v+f2v}}

  // f3.cancel()
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
  
  f1.addChangeListener(() => log("f1 is obsolete"))
  f2.addChangeListener(() => log("f2 is obsolete"))
  f3.addChangeListener(() => log("f3 is obsolete"))
    
  Thread.sleep(2000);
  f2Obsoleter.change()
  Thread.sleep(1000);
  f3.dispose()
  Thread.sleep(1000);
  
  log("Program terminating");
    
}