package greeter

import org.scalatest.FunSuite

/**
 * Created by luckcheese on 5/5/14.
 */
class HelloTest extends FunSuite {
  test("sayHello method works correctly") {
    val hello = new Hello
    assert(hello.sayHello("Scala") == "Hello, Scala!")
  }
}
