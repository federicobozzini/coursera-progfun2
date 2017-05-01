package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("delta for different values") {
    val a, b, c = Var(0.0)
    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() == 0)

    b() = 2
    assert(delta() == 4)

    c() = 1
    assert(delta() == 4)

    a() = 0.5
    assert(delta() == 2)

    b() = 4
    assert(delta() == 14)

    a() = 7/4.0
    assert(delta() == 9)
  }

  test("polynomials.computeSolutions") {
    val a, b, c = Var[Double](0)
    val delta = Polynomial.computeDelta(a, b, c)
    val solutions = Polynomial.computeSolutions(a, b, c, delta)
    assert(solutions().isEmpty)

    b() = 2
    assert(solutions() == Set(0))

    c() = 1
    assert(solutions() == Set(-0.5))

    a() = 0.5
    assert(solutions() == Set(-2-Math.sqrt(2),-2+Math.sqrt(2)))

    b() = 4
    a() = 7/4.0
    assert(solutions() == Set[Double](-2, -2/7.0))
  }


}
