package com.github.shoujikazuo.scala_calculator.core.lexer

import org.scalatest.FreeSpec

class StringLexerTest extends FreeSpec {

  def templateTest(aExp: String): Unit = {
    val lexer = new StringLexer()
    lexer.parse(aExp) match {
      case Left(tokens) =>
        System.out.println(tokens)
      case Right(err) =>
        System.err.println(err)
        assert(false)
    }
  }

  "Methods tests" - {
    "parse" - {
      "スペースを含まない正しい数式をパースできる" in {
        templateTest("34+5*6/7")
      }
      "スペースを含む正しい数式をパースできる" in {
        templateTest("56 + 7 * 8 /  9")
      }
    }
  }
}
