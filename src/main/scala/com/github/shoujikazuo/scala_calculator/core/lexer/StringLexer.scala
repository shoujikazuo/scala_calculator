package com.github.shoujikazuo.scala_calculator.core.lexer

class StringLexer extends Lexer {
  type LexerInputType = String
  type LexerOutputType = Tokens[String, TokenType]
  type LexerErrorType = LexerError

  override def parse(aSource: LexerInputType): Either[LexerOutputType, LexerErrorType] = {
    return parseImpl(aSource, 0, Seq())
  }

  private[this] def parseImpl(aSource: LexerInputType, aStartIdx: Int, aAccumulatedTokens: Seq[Token[String,TokenType]]): Either[LexerOutputType, LexerErrorType] = {
    if(aStartIdx > aSource.length-1) {
      return Left(Tokens(aAccumulatedTokens))
    }

    def constructToken(aSource: String, aStartIdx: Int, aTokenType: TokenType, aShouldIncludeToToken: Char => Boolean): (Int, Token[String, TokenType]) = {
      def impl(aTokenStr: String, aSource: String, aCurrentIdx: Int, aTokenType: TokenType, aShouldIncludeToToken: Char => Boolean): (Int, Token[String, TokenType]) = {
        return if(aCurrentIdx < aSource.length && aShouldIncludeToToken(aSource.charAt(aCurrentIdx))) {
          impl(aTokenStr + aSource.charAt(aCurrentIdx), aSource, aCurrentIdx + 1, aTokenType, aShouldIncludeToToken)
        } else {
          (aCurrentIdx, Token(aTokenStr, aTokenType))
        }
      }
      return impl("", aSource, aStartIdx, aTokenType, aShouldIncludeToToken)
    }

    def parseNumericToken(aSource: String, aStartIdx: Int): (Int, Either[Token[String, TokenType], LexerErrorType]) = {
      val (nextIdx, token) = constructToken(aSource, aStartIdx, NumericTokenType, {ch =>
        ch.isDigit
      })
      return (nextIdx, Left(token))
    }
    def parseMulOperatorToken(aSource: String, aStartIdx: Int): (Int, Either[Token[String, TokenType], LexerErrorType]) = {
      val (nextIdx, token) = constructToken(aSource, aStartIdx, MulOperatorTokenType, {ch =>
        MulOperatorTokenType.isValid(ch)
      })
      return (nextIdx, Left(token))
    }
    def parseDivOperatorToken(aSource: String, aStartIdx: Int): (Int, Either[Token[String, TokenType], LexerErrorType]) = {
      val (nextIdx, token) = constructToken(aSource, aStartIdx, DivOperatorTokenType, {ch =>
        DivOperatorTokenType.isValid(ch)
      })
      return (nextIdx, Left(token))
    }
    def parsePlusOperatorToken(aSource: String, aStartIdx: Int): (Int, Either[Token[String, TokenType], LexerErrorType]) = {
      val (nextIdx, token) = constructToken(aSource, aStartIdx, PlusOperatorTokenType, {ch =>
        PlusOperatorTokenType.isValid(ch)
      })
      return (nextIdx, Left(token))
    }
    def parseMinusOperatorToken(aSource: String, aStartIdx: Int): (Int, Either[Token[String, TokenType], LexerErrorType]) = {
      val (nextIdx, token) = constructToken(aSource, aStartIdx, MinusOperatorTokenType, {ch =>
        MinusOperatorTokenType.isValid(ch)
      })
      return (nextIdx, Left(token))
    }
    def parseSpaceToken(aSource: String, aStartIdx: Int): (Int, Either[Token[String, TokenType], LexerErrorType]) = {
      val (nextIdx, token) = constructToken(aSource, aStartIdx, SpaceTokenType, {ch =>
        ch.isSpaceChar
      })
      return (nextIdx, Left(token))
    }

    val (tNextIdx, tTokenOrError) = aSource.charAt(aStartIdx) match {
      case x if x.isDigit => parseNumericToken(aSource, aStartIdx)
      case x if MulOperatorTokenType.isValid(x) => parseMulOperatorToken(aSource, aStartIdx)
      case x if DivOperatorTokenType.isValid(x) => parseDivOperatorToken(aSource, aStartIdx)
      case x if PlusOperatorTokenType.isValid(x) => parsePlusOperatorToken(aSource, aStartIdx)
      case x if MinusOperatorTokenType.isValid(x) => parseMinusOperatorToken(aSource, aStartIdx)
      case x if x.isSpaceChar => parseSpaceToken(aSource, aStartIdx)
      case x => (0, Right(LexerError("Invalid character: " + x)))
    }

    return tTokenOrError match {
      case Right(error) => Right(error)
      case Left(token: Token[String, TokenType]) => parseImpl(aSource, tNextIdx, aAccumulatedTokens :+ token)
    }
  }
}
