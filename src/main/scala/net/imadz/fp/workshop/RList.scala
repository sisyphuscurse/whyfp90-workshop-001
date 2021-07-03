package net.imadz.fp.workshop

import scala.annotation.tailrec

// Session 1: Higher Order Functions with List and List Combinator Functions
sealed trait RList[+T] {
  def head: T
  def tail: RList[T]
}
case object RNil extends RList[Nothing] {
  override def head: Nothing = ???
  override def tail: RList[Nothing] = RNil
}
class RCons[+T](x: T, xs: => RList[T]) extends RList[T] {
  override def head: T = x
  override def tail: RList[T] = xs
  override def toString: String = s"RCons(${x}, ?)"
}
object RCons {
  def apply[T](x: T, xs: RList[T]): RList[T] = new RCons(x, xs)
  def unapply[T](xs: RCons[T]): Option[(T, RList[T])] = Some((xs.head, xs.tail))
}

object RList {
  // 1. Define List[+T] ADT as Nil | Cons
  // 2. Implement sum, product, anyTrue, allTrue
  //  def sum: RList[Int] => Int = {
  //    case RNil         => 0
  //    case RCons(h, t)  => h + sum(t)
  //  }
  //  def product2: RList[Int] => Int = {
  //    case RNil => 1
  //    case RCons(h, t) => h * product2(t)
  //  }
  // (A, B) => B  = A => (B => B)
  // 1 + 2 = 3, (Int, Int) => Int
  // () + 2 => 3, Int => (Int => Int) == Int => Int => Int
  // 3. Extract foldr combinator function
  @tailrec
  def foldr[A, B](zero: B)(f: A => B => B)(xs :RList[A]): B = xs match {
    case RNil => zero
    case RCons(h, t) => foldr(f(h)(zero))(f)(t)
  }

  def range(from: Int, to: Int): RList[Int] = {
    @tailrec
    def go(from: Int, to: Int, acc: RList[Int]): RList[Int] = {
      if (from >= to) RCons(from, acc)
      else go(from, to - 1, RCons(to, acc))
    }
    go(from, to, RNil)
  }
  // 4. Express sum, product, anyTrue, and allTrue with foldr function
  def sum: RList[Int] => Int                                        = foldr[Int, Int](0)(x => y => x + y)
  def product: RList[Int] => Int                                    = foldr[Int, Int](1)(x => y => x * y)
  def anyTrue: RList[Boolean] => Boolean                            = foldr[Boolean, Boolean](false)(x => y => x || y)
  def allTrue: RList[Boolean] => Boolean                            = foldr[Boolean, Boolean](true)(x => y => x && y)
  // 5. Define append combinator function
  def cons[T]: T => RList[T] => RList[T]                            = head => tail => RCons[T](head, tail)
  def append[T](xs: RList[T])(ys: RList[T]): RList[T]               = foldr[T, RList[T]](ys)(cons)(xs)
  // 6. Define doubleAll function
  def doubleAll: RList[Int] => RList[Int]                           = foldr[Int, RList[Int]](RNil)(doubleAndCons)
  private def doubleAndCons: Int => RList[Int] => RList[Int]        = fAndCons(double)
  def double: Int => Int = _ * 2
  private def fAndCons[A, B](f: A => B): A => RList[B] => RList[B]  = f andThen cons
  // 7. Extract map combinator function
  // [1, 2, 3, 4, 5] => ["a", "b", "c", "d", "e"]
  // f: 1 -> a, 2 -> b, ..., 5 -> e
  def map[A, B](f: A => B): RList[A] => RList[B]                    = foldr[A, RList[B]](RNil)(fAndCons(f))
  // 8. Define tail recursive version foldr to fix stack overflow error
  def foreach[A](f: A => Unit): RList[A] => Unit = foldr[A, Unit](())(x => _ => f(x))
  // 9. Define lazy evaluation version List

  // paradigm vs pattern
}

object RListDemo extends App {

  import RList._

  println("Just a List Demo")
  // 1. Express some int/boolean list with RList
  // []
  println(RNil)
  // [1]
  println(RCons(1, RNil))
  // [1, 2]
  val shortInts = RCons(1, RCons(2, RNil))
  println(shortInts)

  // [1, 2, 3, 4,
  val longInts: RList[Int] = RCons(1, RCons(2, RCons(3, RCons(4, RCons(5, RNil)))))
  println(longInts)
  // [true, false, true, false, false]
  val longBooleans: RList[Boolean] = RCons(true, RCons(false, RCons(true, RCons(false, RCons(false, RNil)))))
  println(longBooleans)
  // 2. Try to sum/product/anyTrue/allTrue those int list above
  println(sum(shortInts))
  println(sum(longInts))

  println(product(shortInts))
  println(product(longInts))

  println(anyTrue(longBooleans))
  println(allTrue(longBooleans))

  println(append(shortInts)(longInts))
  // 3. Try to append some list above
  // 4. Try to doubleAll some int lists above
  println(doubleAll(longInts))
  // curry -> curried
  val add: Int => Int => Int = x => y => x + y
  val add2: Int => Int = add(1)
  val add3: Int = add2(2)
  val add4: Int => Int = x => add2(x) * 2
  val add5: Int => Int = add2 andThen double
  val add6: Int => Int = double compose add2 // double . add2
  println(add3)
  // 5. Try to sum 2-dimension int matrix with combinators in RList
  val intMatrix: RList[RList[Int]] =
    RCons(RCons(1, RCons(2, RCons(3, RNil))),
      RCons(RCons(4, RCons(5, RCons(6, RNil))),
        RCons(RCons(7, RCons(8, RCons(9, RNil))),
          RCons(RCons(10, RCons(11, RCons(12, RNil))), RNil))))

  // RList[RList[Int]] => RList[Int] => Int
  // RList[RList[Int]] -in-> CHIP_1 ->RList[Int]-> CHIP_2 -out-> Int
  // map(sum) andThen sum
  def sumMatrix: RList[RList[Int]] => Int = map(sum) andThen sum
  // 6. Try to sum or foreach(println) a very long RList with a stack overflow error
  val longLongInts = range(1, 10000)
  val xs = doubleAll(longLongInts)
  foreach(println)(xs)
  println(xs)
}

// Session 2: Higher Order Functions with Node Label Tree and Tree Combinator Functions
object RNode {
  // 1. Define RNode ADT with RList constructs, Node[+T](label:T, subtrees: RList[Node[T]])
  // 2. Define foldTree combinator
  // 3. Define mapTree combinator
  // 4. Define labels function
}

object RNodeDemo extends App {
  // 1. Express some int node label tree with RNode
  // 2. Try to sum the node label tree with foldTree
}