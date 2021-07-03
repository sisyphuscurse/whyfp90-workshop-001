package net.imadz.fp.workshop

import net.imadz.fp.workshop.List.{append, cons, nil}

import scala.annotation.tailrec


// paradigm vs pattern
// Session 1: Higher Order Functions with List and List Combinator Functions
sealed trait List[+T] {
  def head: T
  def tail: List[T]
}
case object Nil extends List[Nothing] {
  override def head: Nothing = throw new UnsupportedOperationException("Nil does not support this operation")
  override def tail: List[Nothing] = Nil
}
class ::[+T](h: T, t: => List[T]) extends List[T] {
  override def head: T = h
  override def tail: List[T] = t
}
object :: {
  def apply[T](head: T, tail: => List[T]): List[T] = new ::(head, tail)
  def unapply[T](cons: ::[T]): Option[(T, List[T])] = Some((cons.head, cons.tail))
}
object List {
  // 1. Define List[+T] ADT as Nil | Cons
  def nil[T]: List[T] = Nil
  def cons[T]: T => List[T] => List[T] = x => xs => ::(x, xs)
  // 2. Implement sum, product, anyTrue, allTrue
  val _sum: List[Int] => Int = {
    case Nil => 0
    case ::(head, tail) => head + sum(tail)
  }
  // 3. Extract foldr combinator function
  @tailrec
  def foldr[A, B](zero: B)(f: A => B => B)(xs: List[A]): B =  xs match {
    case Nil => zero
    case ::(head, tail) => foldr(f(head)(zero))(f)(tail)
  }
  // 4. Express sum, product, anyTrue, and allTrue with foldr function
  def add: Int => Int => Int = x => y => x + y
  def multiply: Int => Int => Int = x => y => x * y
  def or: Boolean => Boolean => Boolean = a => b => a || b
  def and: Boolean => Boolean => Boolean = a => b => a && b

  def sum: List[Int] => Int                              = foldr(0)(add)
  def product: List[Int] => Int                          = foldr(1)(multiply)
  def anyTrue: List[Boolean] => Boolean                  = foldr(false)(or)
  def allTrue: List[Boolean] => Boolean                  = foldr(true)(and)

  // 5. Define append combinator function
  def append[T](xs: List[T])(ys: List[T]): List[T]       = foldr(ys)(cons)(xs)
  // 6. Define doubleAll function
  def doubleAll: List[Int] => List[Int]                  = foldr(nil[Int])(doubleandcons)
  def doubleandcons: Int => List[Int] => List[Int]       = fandcons(double)
  def fandcons[A, B](f: A => B): A => List[B] => List[B] = cons compose f
  def double: Int => Int = _ * 2
  // 7. Extract map combinator function
  def map[A, B](f: A => B): List[A] => List[B]           = foldr(nil[B])(fandcons(f))
  // 8. Define tail recursive version foldr to fix stack overflow error
  def summatrix: List[List[Int]] => Int                  = sum compose map(sum)
  // 9. Define lazy evaluation version List
  def range(from: Int, to: Int): List[Int]               = {
    @tailrec
    def go(from: Int, to: Int, acc: List[Int]): List[Int] = {
      if (from >= to) ::(to, acc)
      else go(from, to - 1, ::(to, acc))
    }
    go(from, to, Nil)
  }

}

object ListDemo extends App {

  import List._

  println("Just a List Demo")
  // 1. Express some int/boolean list with RList
  // []
  // [1]
  // [1, 2]
  // [1, 2, 3, 4]
  // [true, false, true, false, false]
  // 2. Try to sum/product/anyTrue/allTrue those int list above
  // 3. Try to append some list above
  // 4. Try to doubleAll some int lists above
  // 5. Try to sum 2-dimension int matrix with combinators in RList
  // map(sum) andThen sum
  // 6. Try to sum or foreach(println) a very long RList with a stack overflow error
}

// Session 2: Higher Order Functions with Node Label Tree and Tree Combinator Functions
class Node[+T](l: T, sub: => List[Node[T]]) {
  def label: T = l
  def subtrees: List[Node[T]] = sub
}
object Node {
  // 1. Define RNode ADT with RList constructs, Node[+T](label:T, subtrees: RList[Node[T]])
  def apply[T](label: T, subtrees: => List[Node[T]]): Node[T]              = new Node(label, subtrees)
  def unapply[T](node: Node[T]): Option[(T, List[Node[T]])]                = Some((node.label, node.subtrees))
  // 2. Define foldTree combinator
  def foldTree[X, YS, Y](f: X => YS => Y)(g: Y => YS => YS)(zero: YS): Node[X] => Y   = {
    case Node(label, subtrees) => f(label)(foldTrees(f)(g)(zero)(subtrees))
  }
  def foldTrees[X, YS, Y](f: X => YS => Y)(g: Y => YS => YS)(zero: YS): List[Node[X]] => YS = {
    case Nil => zero
    case ::(head, tail) => g(foldTree(f)(g)(zero)(head))(foldTrees(f)(g)(zero)(tail))
  }

  // 3. Define mapTree combinator
  def mapTree[A, B](f: A => B): Node[A] => Node[B]                          = foldTree(fandcons2(f))(cons)(nil[Node[B]])
  def node[T]: T => List[Node[T]] => Node[T]                                = x => xs => Node(x, xs)
  def fandcons2[A, B](f: A => B): A => List[Node[B]] => Node[B]             = node compose f

  // 4. Define labels function
  def labels[T]: Node[T] => List[T]                                         = foldTree(cons[T])(append)(nil[T])
}

object NodeDemo extends App {
  // 1. Express some int node label tree with RNode
  // 2. Try to sum the node label tree with foldTree
}