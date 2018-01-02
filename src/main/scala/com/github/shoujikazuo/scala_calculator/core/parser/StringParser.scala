package com.github.shoujikazuo.scala_calculator.core.parser

import com.github.shoujikazuo.scala_calculator.core.lexer._

class StringParser extends Parser {
  type ParserInputType
  type ParserOutputType
  type ParserErrorType

  override def parse(aSource: ParserInputType): Either[ParserOutputType, ParserErrorType] = {
    ???
  }

  /**
    * 再帰下降構文解析を行なうパーサ．
    * 文法は以下:
    * {{{
    *   expression        ::= term { plusminusoperator term }*
    *   term              ::= factor { muldivoperator factor }*
    *   factor            ::= number
    *   number            ::= digit | number { digit }*
    *   muldivoperator    ::= * | /
    *   plusminusoperator ::= + | -
    *   digit             ::= 0 | 1 | ... | 9
    * }}}
    *
    * @param allTokens
    * @tparam A
    * @tparam B
    */
  private[this] class RecursiveDescentParser[A, +B](allTokens: Tokens[A,B]) {

    private[this] case class RDPInnerResult(tree: Tree[Token[A,B]], nextTokenIdx: Int)

    def parse(): Either[Tree[A], ParserErrorType] = {
      ???
    }

    def parseExpression[B1 >: B](tokens: Seq[Token[A,B1]], currentIdx: Int): Either[RDPInnerResult, ParserErrorType] = {
      parseTerm(tokens, currentIdx) match {
        case Left(leftTerm) =>
          if(leftTerm.nextTokenIdx > tokens.length) {
            return Left(leftTerm)
          }
          val nextToken = tokens(leftTerm.nextTokenIdx)
          nextToken.tpe match {
            case PlusOperatorTokenType | MinusOperatorTokenType =>
              parseTerm(tokens, leftTerm.nextTokenIdx + 1) match {
                case Left(rightTerm) =>
                  // construct tree
                  val newTree = Node[Token[A, B1]](nextToken, leftTerm.tree, rightTerm.tree)
                  Left(RDPInnerResult(newTree, leftTerm.nextTokenIdx + 1))
                case Right(error) =>
              }
            case _ =>
          }

        case Right(error) => Right(error)
      }


      ???
    }

    def parseTerm[B1 >: B](tokens: Seq[Token[A,B1]], currentIdx: Int): Either[RDPInnerResult, ParserErrorType] = {
      ???
    }

    def parseFactor[B1 >: B](tokens: Seq[Token[A,B1]], currentIdx: Int): Either[RDPInnerResult, ParserErrorType] = {
      ???
    }

    def parseNumber[B1 >: B](tokens: Seq[Token[A,B1]], currentIdx: Int): Either[RDPInnerResult, ParserErrorType] = {
      ???
    }

    def parseMulDivOperator[B1 >: B](tokens: Seq[Token[A,B1]], currentIdx: Int): Either[RDPInnerResult, ParserErrorType] = {
      ???
    }

    def parsePlusMinusOperator[B1 >: B](tokens: Seq[Token[A,B1]], currentIdx: Int): Either[RDPInnerResult, ParserErrorType] = {
      ???
    }
  }
}

