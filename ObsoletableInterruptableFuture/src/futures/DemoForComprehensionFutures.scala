package futures

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

/**
 * Show how for comprehensions can be used with futures, and how scala
 * futures effectively give you referential transparency for much of your code.
 */
object DemoForComprehensionFutures extends App {
  
  val started = System.currentTimeMillis();
  def log(s:String) = println("Time : %4d => %s".format(System.currentTimeMillis()-started,s))

  
  // Scala futures came in 2.10
  
  def expensive() : Future[Int]= future {
    Thread.sleep(1000);
    42
  }
  
  
  val f1 : Future[Int] = expensive() 
  val f2 : Future[Int] = expensive() 
  val f3 : Future[Int] = for (f1v<-f1;f2v<-f2) yield f1v+f2v
    // translates to 
  val f3a : Future[Int] = f1.flatMap{f1v=>for (f2v<-f2) yield f1v+f2v}
  // translates to 
  val f3b : Future[Int] = f1.flatMap{f1v=>f2.map{f2v => f1v+f2v}}

  log("Future assigned to f");
  
  f1 onComplete {
    case Success(i) => log("Got f1 result "+i)
    case Failure(t) => t.printStackTrace();
  }
  
  f2 onComplete {
    case Success(i) => log("Got f2 result "+i)
    case Failure(t) => t.printStackTrace();
  }
  
  f3 onComplete {
    case Success(i) => log("Got f3 result "+i)
    case Failure(t) => t.printStackTrace();
  }
  
  Thread.sleep(3000);
  log("Program terminating");
    
      // sensible next file to look at
  import futures.DemoForComprehensionFuturesAlternateExecutor

}