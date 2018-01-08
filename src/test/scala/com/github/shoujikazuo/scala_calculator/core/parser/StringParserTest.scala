package com.github.shoujikazuo.scala_calculator.core.parser

import com.github.shoujikazuo.scala_calculator.core.lexer.StringLexer
import org.scalatest.FreeSpec

class StringParserTest extends FreeSpec {

  private[this] def templateTest(aExp: String) = {
    val lexer = new StringLexer()
    lexer.parse(aExp) match {
      case Left(tokens) =>
        val parser = new StringParser()
        System.out.println(parser.parse(tokens))
      case Right(errorOnLexer) => assert(false, "Error on lexer: " + errorOnLexer)
    }
  }

  "Methods tests" - {
    "parse" - {
      "正しくパースできるTokenを渡すと構文木が返る" in {
        templateTest("34+5*6/7")
      }
      "正しくパースできるToken(スペース区切り)を渡すと構文木が返る" in {
        templateTest("34 + 5 * 6 / 7")
      }
    }
  }
}
