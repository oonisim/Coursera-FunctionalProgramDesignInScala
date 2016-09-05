/*
 * Coursera Functional Program Design in Scala
 * WEEK 04 Timely Effects assignment.
 * https://www.coursera.org/learn/progfun2/programming/sO8Cf/calculator
 */
package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    //Signal(Math.max((MaxTweetLength - tweetLength(tweetText())), 0))
    Signal(MaxTweetLength - tweetLength(tweetText()))
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    val THRESHOLD_GREEN = 15
    val THRESHOLD_RED = 0
    val COLOR_GREEN = "green"
    val COLOR_RED = "red"
    val COLOR_ORANGE = "orange"
    Signal(
    remainingCharsCount() match {
      case x if x >= THRESHOLD_GREEN => COLOR_GREEN 
      case x if x >= THRESHOLD_RED   => COLOR_ORANGE
      case other => COLOR_RED
    })
  }

  /**
   * Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
        (Character.isSurrogatePair _).tupled)
    }
  }
}
