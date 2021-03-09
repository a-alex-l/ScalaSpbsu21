package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList
}

case class IntNil() extends IntList {
  override def head: Int = undef
  override def tail: IntList = undef
  override def drop(n: Int): IntList = undef
  override def take(n: Int): IntList = undef
  override def map(f: Int => Int): IntList = this
  override def ::(elem: Int): IntList = IntList(Seq(elem))
}

case class IntMore(seq: Seq[Int]) extends IntList {
  override def head: Int = seq.head
  override def tail: IntList = IntList(seq.tail)
  override def drop(n: Int): IntList =
    if (seq.size < n) { undef } else { IntList(seq.drop(n)) }
  override def take(n: Int): IntList =
    if (seq.size < n) { undef } else { IntList(seq.take(n)) }
  override def map(f: Int => Int): IntList = IntList(seq.map(f))
  override def ::(elem: Int): IntList = IntList(Seq(elem) ++ seq)
}

object IntList {
  def apply(seq: Seq[Int]): IntList =
    if (seq.isEmpty) { IntNil() } else {IntMore(seq) }
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = IntList(seq)
  def sum(intList: IntList): Int =
    intList match {
      case IntNil() => undef
      case _ => foldLeft ((currentSum: Int, now: Int) => currentSum + now, 0, intList)
    }
  def size(intList: IntList): Int = foldLeft((currentCount: Int, now: Int) => currentCount + 1, 0, intList)
  @tailrec
  def foldLeft(f: (Int, Int) => Int, startValue: Int, intList: IntList): Int = {
    intList match {
      case IntNil() => startValue
      case _ => foldLeft(f, f(startValue, intList.head), intList.tail)
    }
  }
}
