package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
    def head: Int
    def tail: IntList
    def drop(n: Int): IntList
    def take(n: Int): IntList
    def map(f: Int => Int): IntList
    def ::(elem: Int): IntList = IntMore(elem, this)
}

case class IntNil() extends IntList {
    override def head: Int = undef
    override def tail: IntList = undef
    override def drop(n: Int): IntList = if (n == 0) { this } else { undef }
    override def take(n: Int): IntList = if (n == 0) { this } else { undef }
    override def map(f: Int => Int): IntList = this
}

case class IntMore(value: Int, prev: IntList) extends IntList {
    override def head: Int = value
    override def tail: IntList = prev
    override def drop(n: Int): IntList =
        if (n == 0) { this } else { prev.drop(n - 1) }
    override def take(n: Int): IntList =
        if (n == 0) { IntNil() } else { IntMore(value, prev.take(n - 1)) }
    override def map(f: Int => Int): IntList = IntMore(f(value), prev.map(f))
}

object IntList {
    def apply(seq: Seq[Int]): IntList =
        if (seq.isEmpty) { IntNil() } else { IntMore(seq.head, IntList(seq.tail)) }
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