package futures


import scala.concurrent._
import scala.util.{Success, Failure}


/**
 * Very simple demo of a future.
 */
object DemoFutures extends App {
  
  val started = System.currentTimeMillis();
  def log(s:String) = println("Time : %4d => %s".format(System.currentTimeMillis()-started,s))

  import ExecutionContext.Implicits.global

  // Scala futures came in 2.10
  
  def expensive() : Future[Int]= future {
    Thread.sleep(1000);
    42
  }
  
  
  val f : Future[Int] = expensive() 
  
  log("Future assigned to f");
  
  f onComplete {
    case Success(i) => log("Got result "+i)
    case Failure(t) => t.printStackTrace();
  }
  
  log("non-blocking onComplete call finished")
  
  import scala.language.postfixOps
  import scala.concurrent.duration._
  val valueF:Int = Await.result(f,2 seconds)

  log("Got value by blocking "+valueF)
  
  Thread.sleep(2000);
  log("Program terminating");
  
  
  // sensible next file to look at - DemoAlternativeFutures which looks at an alternate way of solving the same problem.
  import futures.DemoAlternativeFutures
}