package scalaworld.workshop

import scala.meta._
// These may come in handy.
import scala.meta.tokens.Token.{LeftBrace, LeftBracket, LeftParen, RightBrace, RightBracket, RightParen}

class BalancedSuite extends WorkshopSuite {
  override def run(str: String): Boolean = isBalanced(str.tokenize.get)

  // see https://geirsson.com/post/2016/02/scalameta/#find-correct-matching-parentheses
  def matching(tokens: Seq[Token]): Map[Token, Token] = {
    val result = scala.collection.mutable.Map.empty[Token, Token]
    var stack = List.empty[Token]
    tokens.foreach {
      case open@( _: LeftBrace | _: LeftBracket | _: LeftParen) => stack = open :: stack
      case close@( _: RightBrace | _: RightBracket | _: RightParen) =>
        stack.headOption.map((_, close)) foreach {
          case t@((_:LeftBrace, _:RightBrace) | (_:LeftBracket, _:RightBracket) | (_:LeftParen, _:RightParen)) =>
            result += t._1 -> t._2
            stack = stack.tail
          case _ => return Map.empty
        }
      case _ =>
    }
    result.toMap
  }

  /** Are parentheses balanced? */
  def isBalanced(tokens: Tokens): Boolean = {
    val t = tokens.filter(t => t.isInstanceOf[LeftParen] || t.isInstanceOf[RightParen] || t.isInstanceOf[LeftBracket] || t.isInstanceOf[RightBracket] || t.isInstanceOf[LeftBrace] || t.isInstanceOf[RightBrace])
    matching(t).nonEmpty && (t.length % 2 == 0)
  }

  checkNot("{")
  check("{}")
  checkNot("}{")
  check("{}{}{}")
  checkNot("}{}{}")
  check("[](){}")
  checkNot("[(){}")
  checkNot("[]){}")
  checkNot("[]()}")

  checkNot("(}")
  checkNot("(][)")
  checkNot("{(})")

  checkNot(""" val x = "{" + `{` + }  `}` """)

  check("val x = { 2 }")
  check("""|def x = {
           |  List(1, 2).map { case x => x }
           |}
           |""".stripMargin)
  checkNot("""|def x =
              |  List(1, 2).map { case x => x }
              |}
              |""".stripMargin)
  check("val x = { function(2) }")
  check("""|def foo[T](args: T*): Unit = {
           |  foo(bar(kaz[T](args:_*)))
           |}
           |""".stripMargin)
  checkNot("""|def fooT](args: T*): Unit = {
              |  foo(bar(kaz[T](args:_*)))
              |}
              |""".stripMargin)
  checkNot("""|def fooT](args: T*): Unit = {
              |  foo(bar(kaz[T](args:_*
              |}
              |""".stripMargin)
  checkNot("""|def foo[T](args: T*): Unit = {
              |  foo(bar(kaz[T(args:_*))
              |}
              |""".stripMargin)

}
