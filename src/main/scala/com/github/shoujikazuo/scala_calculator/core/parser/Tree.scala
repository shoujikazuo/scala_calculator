package com.github.shoujikazuo.scala_calculator.core.parser

sealed trait Tree[+A] {
  def nodeValue: A
  def leftNode: Option[Tree[A]]
  def rightNode: Option[Tree[A]]
}

case class Node[+A](private[this] val value: A, private[this] val left: Tree[A], private[this] val right: Tree[A]) extends Tree[A] {
  override def nodeValue: A = value

  override def leftNode = Some(left)

  override def rightNode = Some(right)

  def withLeftNode[A1 >: A](node: Node[A1]) = this.copy(left = node)
  def withRightNode[A1 >: A](node: Node[A1]) = this.copy(right = node)
  def withNewValue[A1 >: A](newVal: A1) = this.copy(value = newVal)
}

case class Leaf[+A](private[this] val value: A) extends Tree[A] {

  override def leftNode = None

  override def rightNode = None

  override def nodeValue = value
}
