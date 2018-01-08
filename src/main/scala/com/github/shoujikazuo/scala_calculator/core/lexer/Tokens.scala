package com.github.shoujikazuo.scala_calculator.core.lexer

sealed trait TokenType

case class Token[+A](value: A, tpe: TokenType)

case class Tokens[+A](values: Seq[Token[A]])

/**
  * End Of Token. Token列の終わり．
  */
case object EOT extends TokenType

/**
  * 構文解析の開始時にプッシュダウンオートマトンへ積まれる開始トークン．
  */
case object StartTokenType extends TokenType

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