package scalaworld.workshop

import scalaworld.util.logger
import scala.meta._

import org.scalatest.FunSuite
import scala.meta.tokens.Token._
import scalaworld.util.DiffAssertions

class TrailingCommaSuite extends FunSuite with DiffAssertions {

  /** Removes all commas behind the last argument of function calls */
  def stripTrailingCommas(tokens: Tokens): String =
    tokens.foldRight(List.empty[Token]){(token, tokens) =>
      tokens match {
        case (_: RightParen) :: _ |
             (_: LF) :: (_: RightParen) :: _ |
             (_: LF) :: (_: Comment) :: (_: LF) :: (_: RightParen) :: _
          if token.isInstanceOf[Comma] => tokens
        case _ => token :: tokens
      }
    }.mkString.tokenize.get.syntax

  def check(original: String, expected: String): Unit = {
    test(logger.reveal(original)) {
      val obtained = stripTrailingCommas(original.tokenize.get)
      assertNoDiff(obtained.trim, expected.trim)
    }
  }

  check(
    """|function(
       | arg1,
       | arg2,
       |)""".stripMargin,
    """|function(
       | arg1,
       | arg2
       |)""".stripMargin
  )

  check(
    """|function(
       |  arg1,
       |// arg2,
       |)""".stripMargin,
    """|function(
       |  arg1
       |// arg2,
       |)""".stripMargin
  )

}
