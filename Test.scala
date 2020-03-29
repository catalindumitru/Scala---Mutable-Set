/**  With Scala 2.12 on Lab machines:

 * In normal circumstances the CLASSPATH is already set for you:

fsc TestTest.scala
scala org.scalatest.run TestTest

 * If you use jar files in your own space:

fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest

 * (Once this is working you can set your CLASSPATH in .bashrc) 
*/
import org.scalatest.FunSuite


class TestTest extends FunSuite{
  var x=0
  test("x=0"){ assert(x===0) }
  x = 1
  test("x=1"){ assert(x===1) }
}


/*
//  Corrected:
class TestTest extends FunSuite{
  var x = 0
  test("x=0"){ 
    x=0
    assert(x===0) 
  }
  test("x=1"){ 
    x=1
    assert(x===1)
  }
}
*/  
