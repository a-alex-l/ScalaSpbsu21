package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

class IntList(seq: Seq[Int]) {
    def head: Int = if (seq.isEmpty) { undef } else { seq.head }
    def tail: IntList = if (seq.size == 1) { IntNil } else { new IntList(seq.tail) }
    def map(f: Int => Int): IntList = if (seq.isEmpty) { IntNil } else { new IntList(seq.map(f)) }
    def ::(elem: Int): IntList = new IntList(Seq(elem) ++ seq)
    def drop(n: Int): IntList =
        if (seq.size < n)       { undef }
        else if (seq.size == n) { IntNil }
        else                    { new IntList(seq.drop(n)) }
    def take(n: Int): IntList =
        if (seq.size < n) { undef }
        else if (n == 0)  { IntNil }
        else              { new IntList(seq.take(n)) }

    def equals(intList: IntList): Boolean = {
        if (this == IntNil || intList == IntNil) {
            this == intList
        } else {
            this.head == intList.head && this.tail.equals(intList.tail)
        }
    }
}

object IntList {
    val IntNil: IntList = new IntList(Seq())
    def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
    def fromSeq(seq: Seq[Int]): IntList = if (seq.isEmpty) { IntNil } else { new IntList(seq) }
    def sum(intList: IntList): Int =
        if (intList == IntNil) { undef }
        else { foldLeft((currentSum: Int, now: Int) => currentSum + now, 0, intList) }
    def size(intList: IntList): Int = foldLeft((currentCount: Int, now: Int) => currentCount + 1, 0, intList)
    @tailrec
    def foldLeft(f: (Int, Int) => Int, startValue: Int, intList: IntList): Int = {
        intList match {
            case IntNil => startValue
            case _ => foldLeft(f, f(startValue, intList.head), intList.tail)
        }
    }
}
