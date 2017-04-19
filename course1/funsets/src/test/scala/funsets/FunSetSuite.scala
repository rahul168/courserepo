package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4: Set = e => e > 0 && e%2 == 0 && e < 101 // Even numbers less than 101
    val s5: Set = e => e > 0 && e%2 != 0 && e < 101 // Odd numbers less than 101
    val s6: Set = e => e > 1 && e < 5 // (2, 3, 4)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("Even number set") {
    new TestSets {
      assert(!contains(s4, 1), "1 is odd")
      assert(contains(s4, 2), "2 is Even")
      assert(contains(s4, 50), "50 is even")
      assert(!contains(s4, 57), "57 is odd")
    }
  }

  test("Odd number set") {
    new TestSets {
      assert(contains(s5, 1), "1 is odd")
      assert(!contains(s5, 2), "2 is Even")
      assert(!contains(s5, 50), "50 is even")
      assert(contains(s5, 57), "57 is odd")
    }
  }
  
  test("Forall") {
    new TestSets {
      assert(forall(s4, e => e > 1 && e < 101 ), "greater than 1 and lesser than 101")
      assert(forall(s5, e => e > 0 && e < 100), "greater than 1 and lesser than 100")      
    }
  }
  
  test("Exists") {
    new TestSets {
      assert(exists(s4, e => e%10 == 0), "Numbers divisible by 10 are also even")
      assert(!exists(s5, e => e%10 == 0), "Numbers divisible by 10 cannot be odd")      
    }
  }
  
  test("Intersect") {
    new TestSets {
      assert(contains(intersect(s4, s6), 2), "2 is an even number")
      assert(contains(intersect(s4, s6), 4), "4 is an even number")
      assert(contains(intersect(s5, s6), 3), "3 is an odd number")      
    }
  }
  
  test("Map") {
    new TestSets {
      val s = map(s6,  e => e*e)
      assert(contains(s, 4), "4 is square of 2")
      assert(contains(s, 9), "9 is square of 3")
      assert(contains(s, 16), "16 is square of 4")      
    }
  }
  
  test("Diff") {
    new TestSets {
      val s = diff(s6, s4)
      assert(contains(s, 3), "3 is not even")
      assert(!contains(s, 2), "2 is even")
      assert(!contains(s, 4), "4 is even")      
    }
  }
  
  test("Filter") {
    new TestSets {
      val s = filter(s4,  e => e%10 == 0)
      assert(contains(s, 10), "10 is expected")
      assert(!contains(s, 12), "12 is not expected")
      assert(contains(s, 90), "90 is expected")      
    }
  }
  

}
