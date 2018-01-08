package com.github.shoujikazuo.scala_calculator.core.parser

trait Parser {
  type ParserInputType
  type ParserOutputType
  type ParserErrorType

  def parse(aSource: ParserInputType): Either[ParserOutputType, ParserErrorType]
}

case class ParserError(description: String)