package futures

import scala.concurrent._
import scala.util.{Try,Success, Failure}

/**
 * Futures could be considered just a wrapper around the ability to have a function be called back when the result is complete.
 * So have a look at what it would be like to do without the future wrapper.
 */
object DemoAlternativeFutures extends App {
  
  val started = System.currentTimeMillis();
  def log(s:String) = println("Time : %4d => %s".format(System.currentTimeMillis()-started,s))

  def expensive(callback:Try[Int]=>Unit) = ExecutionContext.Implicits.global.execute{new Runnable { override def run {
    Thread.sleep(1000);
    callback(Success(42));
  }}}
  
  
  expensive{
    case Success(i) => log("Got result "+i)
    case Failure(t) => t.printStackTrace();
  }
  
  Thread.sleep(2000);
  log("Program terminating");
  
  
  // sensible next file to look at
  import futures.DemoAlternativeFuturesComposition
}