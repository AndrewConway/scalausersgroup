package futures

import scala.concurrent._
import scala.util.{Try,Success, Failure}

/**
 * Show how futures could be combined the explicit way.
 */
object DemoAlternativeFuturesComposition extends App {
  
  val started = System.currentTimeMillis();
  def log(s:String) = println("Time : %4d => %s".format(System.currentTimeMillis()-started,s))

  def expensive(callback:Try[Int]=>Unit) = ExecutionContext.Implicits.global.execute{new Runnable { override def run {
    Thread.sleep(1000);
    callback(Success(42));
  }}}
  
  
  def combined(callback:Try[Int]=>Unit) {
    expensive{
      case Success(i1) => 
        log("Got result "+i1)
        expensive{
          case Success(i2) => 
            log("Got result "+i2)
            callback(Success(i1+i2))
          case Failure(t) => t.printStackTrace();
        }
      case Failure(t) => t.printStackTrace();
    }
  }
  
  
  combined{
    case Success(i) => log("Got result "+i)
    case Failure(t) => t.printStackTrace();
  }
  
  Thread.sleep(3000);
  log("Program terminating");
  
  
    // sensible next file to look at
  import futures.DemoForComprehensionOption
}