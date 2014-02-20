package futures

/**
 * Demo of two different ways of holding a potentially non-existant string
 *  1) classical Java way - a string that may be null
 *  2) classical Scala way - an Option[String]
 *  
 * The point of this is to get out some rants^H^H^H^H^H^H^H^H^H^H^H^H^H demonstrate 
 * how for comprehensions are useful outside of collections.
 */

object DemoForComprehensionOption extends App {
  
  val s1 : String = "fred"; // may be null
  
  val s2 : Option[String] = Some("fred") // arg to Some should not be null.
  
  
  def f(s:String) { println(s) }
  def g(s:String) : String = s+s;
  
  
  
  
  
  
  // do something to s1 if not null
  
  if (s1!=null) f(s1)
  
  // 4 different ways of doing the corresponding thing using Option s
  
  if (s2.isDefined) f(s2.get)
  
  s2 match {
    case Some(s2actual) => f(s2actual)
    case None =>
  }
  
  s2.foreach{f}
  
  for (s2actual<-s2) f(s2actual)
  
  // <rant>None of those above seem as readable to me as the Java null method. 
  // And having FOUR reasonable ways to do it (and lots of other, less reasonable ways) is the way of PERL</rant>
  
  
  
  
  
  
  // nested computation
  
  def shrink1(s:String) : String = if (s.isEmpty) null else s.substring(1)

  def shrink2(s:String) : Option[String] = if (s.isEmpty) None else Some(s.substring(1))
  
  
  // shrink three times safely
  
  val res1 : String = if (s1==null) null else { 
    val work1 = shrink1(s1);
    if (work1==null) null else {
      val work2=shrink1(work1);
      if (work2==null) null else shrink1(work2)
    }
  } // hope I didn't miss anything...
  
  val res2 : Option[String] = for ( 
      work0 <- s2;  
      work1 <- shrink2(work0);  
      work2 <- shrink2(work0); 
      work3 <- shrink2(work2) 
    ) yield work3
    
    // which is syntactic sugar for ...
  val res3 : Option[String] = s2.flatMap{work0=>shrink2(work0).flatMap{work1=>shrink2(work0).flatMap{work2=>shrink2(work2).map{work3=>work3}}}} 


    
   // <enlightenment>
   //   Ahh! That is much easier to read than the Java equivalent
   //   <rant>apart from the implication of multiple answers</rant> 
   // </enlightenment> 
    
    
    // sensible next file to look at
  import futures.DemoForComprehensionFutures

  

}