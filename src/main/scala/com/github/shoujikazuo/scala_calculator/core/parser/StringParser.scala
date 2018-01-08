package com.github.shoujikazuo.scala_calculator.core.parser

import com.github.shoujikazuo.scala_calculator.core.lexer._

import scala.collection.immutable.Stack

class StringParser extends Parser {
  type ParserInputType = Tokens[String]
  type ParserOutputType = BinaryTree[Token[String]]
  type ParserErrorType = ParserError

  override def parse(aSource: ParserInputType): Either[ParserOutputType, ParserErrorType] = {
    val rdp = new RecursiveDescentParser[String]()
    rdp.parse(aSource)
  }

  private[this] case class PushDownAutomataBasedParser() {

    def parse(tokens: Seq[Token[String]]): Either[BinaryTree[Token[String]], ParserErrorType] = {
      val stateMachine = StateMachine()
      stateMachine.run(tokens)
    }

    private[this] case class StateMachine() {
      def run(tokens: Seq[Token[String]]): Either[BinaryTree[Token[String]], ParserErrorType] = {
        //FIXME var使いたくないマン
        var state: StateType = Start
        var tree: BinaryTree[Token[String]] = null //FIXME null許さない警察
        var tokenStack = Stack[Token[String]]().push(Token("", StartTokenType))

        for(token <- tokens) {
          val transitedResult = state.transitionFunc(token, tokenStack, tree)
          //FIXME ↓ キモいので書き直したい．
          //FIXME   あとfor() { ... }じゃなくていい感じに書けないのか
          state = transitedResult.stateType
          tokenStack = transitedResult.stack
          tree = transitedResult.tree
        }
        state match {
          case Success =>
            Left(tree) //FIXME stateとtreeってもっと密に結合させるべきなのか...?
          case _ =>
            val errorDescription = state match {
              case Failure(description) => description
              case _ => "今どこにいるの? " + state //FIXME エラーメッセージ
            }
            Right(ParserError(errorDescription))
        }
      }
    }

    private[this] case class TransitionResult(stateType: StateType, stack: Stack[Token[String]], tree: BinaryTree[Token[String]])
    private[this] sealed trait StateType {
      def transitionFunc(token: Token[String], stack: Stack[Token[String]], tree: BinaryTree[Token[String]]): TransitionResult
    }
    private[this] case object Start extends StateType {
      override def transitionFunc(token: Token[String], stack: Stack[Token[String]], tree: BinaryTree[Token[String]]): TransitionResult = {
        token.tpe match {
          case NumericTokenType => TransitionResult(AcceptNumeric, stack, Leaf(token))
          case EOT => TransitionResult(Success, stack, tree)
          case _ => TransitionResult(Failure(""), stack, Leaf(token)) //FIXME エラーメッセージ
        }
      }
    }
    private[this] case object AcceptNumeric extends StateType {
      override def transitionFunc(token: Token[String], stack: Stack[Token[String]], tree: BinaryTree[Token[String]]): TransitionResult = {
        token.tpe match {
          case PlusOperatorTokenType | MinusOperatorTokenType => TransitionResult(AcceptPlusMinusOperator, stack.push(token), Node(token, Some(tree), None))
          case MulOperatorTokenType | DivOperatorTokenType => TransitionResult(AcceptMulDivOperator, stack.push(token), Node(token, Some(tree), None))
          case EOT => TransitionResult(Success, stack, tree)
          case _ => TransitionResult(Failure(""), stack, Leaf(token)) //FIXME エラーメッセージ
        }
      }
    }
    private[this] case object AcceptMulDivOperator extends StateType {
      override def transitionFunc(token: Token[String], stack: Stack[Token[String]], tree: BinaryTree[Token[String]]): TransitionResult = {
        token.tpe match {
          case NumericTokenType if stack.top.tpe == MulOperatorTokenType || stack.top.tpe == DivOperatorTokenType =>
            TransitionResult(AcceptNumeric, stack.pop, tree.withRightNode(Leaf(token)))
          case _ => TransitionResult(Failure(""), stack, Leaf(token)) //FIXME エラーメッセージ
        }
      }
    }
    private[this] case object AcceptPlusMinusOperator extends StateType {
      override def transitionFunc(token: Token[String], stack: Stack[Token[String]], tree: BinaryTree[Token[String]]): TransitionResult = {
        token.tpe match {
          case NumericTokenType if stack.top.tpe == PlusOperatorTokenType || stack.top.tpe == MinusOperatorTokenType =>
            TransitionResult(AcceptNumeric, stack.pop, tree.withRightNode(Leaf(token)))
          case _ => TransitionResult(Failure(""), stack, Leaf(token)) //FIXME エラーメッセージ
        }
      }
    }
    private[this] case class Failure(description: String) extends StateType {
      override def transitionFunc(token: Token[String], stack: Stack[Token[String]], tree: BinaryTree[Token[String]]): TransitionResult
        = TransitionResult(this, stack, tree)
    }
    private[this] case object Success extends StateType {
      override def transitionFunc(token: Token[String], stack: Stack[Token[String]], tree: BinaryTree[Token[String]]): TransitionResult
         = TransitionResult(this, stack, tree)
    }
  }

  /**
    * 再帰下降構文解析を行なうパーサ．
    * 文法は以下:
    * {{{
    *   expression        ::= term { plusminusoperator term }*
    *   term              ::= factor { muldivoperator factor }*
    *   factor            ::= number
    *   number            ::= digit { digit }*
    *   muldivoperator    ::= * | /
    *   plusminusoperator ::= + | -
    *   digit             ::= 0 | 1 | ... | 9
    * }}}
    *
    * @param allTokens
    * @tparam A Tokenの中身の型
    * @tparam B
    */
  private[this] class RecursiveDescentParser[A]() {

    def parse(allTokens: Tokens[A]): Either[BinaryTree[Token[A]], ParserErrorType] = {
      val trimmed = allTokens.values.filter(_.tpe != SpaceTokenType)
      val tokensIter = PushBackedIterator(trimmed)
      parseExpression(tokensIter)
    }

    private[this] def parseExpression(tokensIter: PushBackedIterator[Token[A]]): Either[BinaryTree[Token[A]], ParserErrorType] = {
      import scala.util.control.Breaks
      val b = new Breaks
      var currentTokensIter = tokensIter
      var currentTerm = parseTerm(currentTokensIter)
      currentTerm match {
        case Left(leftTree) =>
            b.breakable {
              while(currentTokensIter.hasNext) {
                val nextToken = currentTokensIter.next()
                nextToken.tpe match {
                  case PlusOperatorTokenType | MinusOperatorTokenType =>
                    val nextTerm = parseTerm(currentTokensIter)
                    nextTerm match {
                      case Left(rightTree) =>
                        currentTerm = Left(Node(nextToken, Some(leftTree), Some(rightTree)))
                      case Right(error) => Right(error)
                    }
                  case _ =>
                    currentTokensIter = currentTokensIter.pushBack(nextToken)
                    b.break
                }
              }
            }
            return currentTerm
        case Right(error) => Right(error)
      }
    }

    private[this] def parseTerm(tokensIter: PushBackedIterator[Token[A]]): Either[BinaryTree[Token[A]], ParserErrorType] = {
      import scala.util.control.Breaks
      val b = new Breaks
      var currentTokensIter = tokensIter
      var currentTerm = parseFactor(currentTokensIter)
      currentTerm match {
        case Left(leftTree) =>
          b.breakable {
            while(currentTokensIter.hasNext) {
              val nextToken = currentTokensIter.next()
              nextToken.tpe match {
                case MulOperatorTokenType | DivOperatorTokenType =>
                  val nextTerm = parseFactor(currentTokensIter)
                  nextTerm match {
                    case Left(rightTree) =>
                      currentTerm = Left(Node(nextToken, Some(leftTree), Some(rightTree)))
                    case Right(error) => Right(error)
                  }
                case _ =>
                  currentTokensIter = currentTokensIter.pushBack(nextToken)
                  b.break
              }
            }
          }
          return currentTerm
        case Right(error) => Right(error)
      }
    }

    private[this] def parseFactor(tokensIter: PushBackedIterator[Token[A]]): Either[BinaryTree[Token[A]], ParserErrorType] = {
      if(tokensIter.hasNext) {
        val nextToken = tokensIter.next()
        nextToken.tpe match {
          case PlusOperatorTokenType | MinusOperatorTokenType | MulOperatorTokenType | DivOperatorTokenType =>
            Right(ParserError("Cannot parse operator on factor: " + nextToken))
          case _ => Left(Leaf(nextToken))
        }
        Left(Leaf(nextToken))
      } else {
        Right(ParserError("No factor in parseFactor()"))
      }
    }

    private[this] def parseNumber(tokensIter: PushBackedIterator[Token[A]]): Either[BinaryTree[Token[A]], ParserErrorType] = {
      if(tokensIter.hasNext) {
        val nextToken = tokensIter.next()
        Left(Leaf(nextToken))
      } else {
        Right(ParserError("No factor in parseFactor()"))
      }
    }
  }

  private[this] case class PushBackedIterator[+A](elems: Seq[A]) {
    private[this] var idx = 0
    def hasNext: Boolean = elems.length > 0 && idx < elems.length
    def next(): A = if(hasNext){
      val elem = elems(idx)
      idx += 1
      elem
    } else {
      Iterator.empty.next()
    }

    def pushBack[A1 >: A](elem: A1): PushBackedIterator[A1] = {
      val newElems = elem +: elems
      PushBackedIterator(newElems)
    }
  }
}

