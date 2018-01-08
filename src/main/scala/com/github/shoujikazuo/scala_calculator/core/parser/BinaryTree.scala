package com.github.shoujikazuo.scala_calculator.core.parser

sealed trait BinaryTree[+A] {
  def value: A
  def leftNode: Option[BinaryTree[A]]
  def rightNode: Option[BinaryTree[A]]
  def withLeftNode[A1 >: A](node: BinaryTree[A1]): BinaryTree[A1]
  def withRightNode[A1 >: A](node: BinaryTree[A1]): BinaryTree[A1]
}

case class Node[+A](override val value: A, override val leftNode: Option[BinaryTree[A]], override val rightNode: Option[BinaryTree[A]]) extends BinaryTree[A] {

  override def withLeftNode[A1 >: A](node: BinaryTree[A1]) = this.copy(leftNode = Some(node))

  override def withRightNode[A1 >: A](node: BinaryTree[A1]) = this.copy(rightNode = Some(node))

  def withNewValue[A1 >: A](newVal: A1) = this.copy(value = newVal)
}

case class Leaf[+A](override val value: A) extends BinaryTree[A] {

  override def leftNode = None

  override def rightNode = None

  override def withLeftNode[A1 >: A](node: BinaryTree[A1]) = Node(this.value, Some(node), None)

  override def withRightNode[A1 >: A](node: BinaryTree[A1]) = Node(this.value, None, Some(node))
}
