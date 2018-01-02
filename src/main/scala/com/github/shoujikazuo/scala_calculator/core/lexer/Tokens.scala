package com.github.shoujikazuo.scala_calculator.core.lexer

sealed trait TokenType

case class Token[A,+B <: TokenType](value: A, tpe: B)

case class Tokens[A,+B <: TokenType](values: Seq[Token[A,B]])

case class LexerError(description: String)

case object NullTokenType extends TokenType
case object NumericTokenType extends TokenType
sealed trait OperatorTokenType extends TokenType {
  def isValid(aToken: Char): Boolean
  protected def isValidImpl(aToken: Char, aExpected: Char): Boolean = {
    aToken == aExpected
  }
}

case object MulOperatorTokenType extends OperatorTokenType {
  override def isValid(aToken: Char): Boolean = {
    isValidImpl(aToken, '*')
  }
}
case object DivOperatorTokenType extends OperatorTokenType {
  override def isValid(aToken: Char): Boolean = {
    isValidImpl(aToken, '/')
  }
}
case object PlusOperatorTokenType extends OperatorTokenType {
  override def isValid(aToken: Char): Boolean = {
    isValidImpl(aToken, '+')
  }
}
case object MinusOperatorTokenType extends OperatorTokenType {
  override def isValid(aToken: Char): Boolean = {
    isValidImpl(aToken, '-')
  }
}

case object SpaceTokenType extends TokenType
case object ErrorTokenType extends TokenType