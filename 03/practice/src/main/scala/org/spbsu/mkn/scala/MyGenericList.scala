package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._
import scala.annotation.tailrec
import scala.Numeric

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[K](f: T => K): MyGenericList[K]
  def ::[K >: T](elem: K): MyGenericList[K] = TMore(elem, this)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef
  override def tail: MyGenericList[Nothing] = undef
  override def drop(n: Int): MyGenericList[Nothing] = if (n == 0) { this } else { undef }
  override def take(n: Int): MyGenericList[Nothing] = if (n == 0) { this } else { undef }
  override def map[K](f: Nothing => K): MyGenericList[K] = fromSeq[K](Seq())
}

case class TMore[T](value: T, prev: MyGenericList[T]) extends MyGenericList[T] {
  override def head: T = value
  override def tail: MyGenericList[T] = prev
  override def drop(n: Int): MyGenericList[T] =
    if (n == 0) { this } else { prev.drop(n - 1) }
  override def take(n: Int): MyGenericList[T] =
    if (n == 0) { MyNil.asInstanceOf[MyGenericList[T]] } else { TMore(value, prev.take(n - 1)) }
  override def map[K](f: T => K): MyGenericList[K] = TMore(f(value), prev.map(f))
}

object MyGenericList {
  def apply[T](seq: Seq[T]): MyGenericList[T] =
    if (seq.isEmpty) { MyNil.asInstanceOf[MyGenericList[T]] } else { TMore(seq.head, MyGenericList(seq.tail)) }
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = MyGenericList(seq)
  def size[T](TList: MyGenericList[T]): Int = foldLeft((currentCount: Int, now: T) => currentCount + 1, 0, TList)
  def sum(TList: MyGenericList[Int]): Int =
    TList match {
      case MyNil => undef
      case _ => foldLeft((currentSum: Int, now : Int) => now + currentSum, TList.head, TList.tail) // Scala is broken
    }
  @tailrec
  def foldLeft[T, K](f: (K, T) => K, startValue: K, TList: MyGenericList[T]): K = {
    TList match {
      case MyNil => startValue
      case _ => foldLeft(f, f(startValue, TList.head), TList.tail)
    }
  }

  def insertInSorted[T](elem : T, list : MyGenericList[T])(implicit comparator : Ordering[T]) : MyGenericList[T] =
    list match {
      case MyNil => fromSeq(Seq(elem))
      case list  => if (comparator.lt(elem, list.head)) elem :: list
                    else list.head :: insertInSorted(elem, list.tail)
  }

  def sort[T](list: MyGenericList[T])(implicit comparator: Ordering[T]): MyGenericList[T] =
    list match {
      case MyNil => MyNil
      case list  => insertInSorted(list.head, sort(list.tail))
    }
}