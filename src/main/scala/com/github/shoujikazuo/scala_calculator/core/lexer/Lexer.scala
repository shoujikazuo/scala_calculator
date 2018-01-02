package com.github.shoujikazuo.scala_calculator.core.lexer

trait Lexer {
  type LexerInputType
  type LexerOutputType
  type LexerErrorType

  def parse(aSource: LexerInputType): Either[LexerOutputType, LexerErrorType]
}

