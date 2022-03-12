package net.imadz

import net.imadz.Node.{labels, mapTree, prune, repeatTree}
import net.imadz.QList._
import net.imadz.QNil.take
import net.imadz.TicTacToeGame.ChessBoard.{Solution, allWinningSolutions}

import scala.::
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

sealed trait QList[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: QList[T]

  def take(n: Int): QList[T]

  def length: Int

}

case object QNil extends QList[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = ???

  override def tail: QList[Nothing] = ???

  override def take(n: Int): QList[Nothing] = QNil

  override def toString: String = "QNil"

  override val length: Int = 0
}

class QCons[+T](x: T, xs: => QList[T]) extends QList[T] {
  override def isEmpty: Boolean = false

  override def head: T = x

  override def tail: QList[T] = xs

  override def take(n: Int): QList[T] = QCons(x, xs.take(n - 1))

  override def toString: String = s"QCons(${x}, ?)"

  override val length: Int = 1 + xs.length
}

object QCons {
  def apply[T](head: T, tail: QList[T]): QList[T] = new QCons(head, tail)

  def unapply[T](xs: QList[T]): Option[(T, QList[T])] =
    if (xs == QNil) None
    else Some((xs.head, xs.tail))
}

object QList {
  def fromArray[T](xs: Array[T]): QList[T] =
    xs.foldRight[QList[T]](QNil) { case (head: T, tail: QList[T]) => cons[T](head)(tail) }

  @tailrec
  def foldr[A, B](zero: B)(f: A => B => B)(s: QList[A]): B = s match {
    case QNil => zero
    case QCons(x, xs) => foldr[A, B](f(x)(zero))(f)(xs)
  }

  @tailrec
  def foldl[A, B](zero: B)(f: B => A => B)(s: QList[A]): B = s match {
    case QNil => zero
    case QCons(x, xs) => foldl(f(zero)(x))(f)(xs)
  }

  def cons[A]: A => QList[A] => QList[A] = x => xs => QCons(x, xs)

  def append[A](xs: QList[A])(ys: QList[A]): QList[A] = foldr(ys)(cons[A])(xs)

  private def fAndCons[A, B](f: A => B): A => QList[B] => QList[B] = f andThen cons[B]

  def map[A, B](f: A => B): QList[A] => QList[B] = foldr[A, QList[B]](QNil)(fAndCons(f))

  def flatten[A]: QList[QList[A]] => QList[A] = foldr[QList[A], QList[A]](QNil)(append)

  def flatMap[A, B](f: A => QList[B]): QList[A] => QList[B] = map(f) andThen flatten[B]

  def reduce[A, B](zero: B)(f: A => B => B): QList[A] => B = foldr(zero)(f)

  def foreach[A](f: A => Unit): QList[A] => Unit = foldl[A, Unit](())(b => a => f(a))

  def count[A](f: A => Boolean): QList[A] => Int = foldr[A, Int](0)(a => c => if (f(a)) c + 1 else c)

  def allTrue: QList[Boolean] => Boolean = foldr[Boolean, Boolean](true)(x => y => x && y)

  def anyTrue: QList[Boolean] => Boolean = foldr[Boolean, Boolean](false)(x => y => x || y)

  def toScala[A]: QList[A] => scala.collection.immutable.List[A] =
    foldr[A, scala.collection.immutable.List[A]](scala.collection.immutable.Nil)(x => xs => x :: xs)

  def fromScala[A]: scala.collection.immutable.List[A] => QList[A] =
    xs => xs.foldRight[QList[A]](QNil)((x, qxs) => cons(x)(qxs))

}

object HigherOrderFunctionListDemo extends App {

  import QList._

  def range(x: Int)(y: Int): QList[Int] = {
    if (x >= y) QCons(x, QNil)
    else cons[Int](x)(range(x)(y - 1))
  }

  // Sum List Test Drive
  val ints: QList[Int] = range(1)(10)
  val intMatrix: QList[QList[Int]] = map(range(1))(ints)
  val sum: QList[Int] => Int = foldr(0)(x => y => x + y)
  println(sum(ints))

  type Quadratic[+T] = QList[QList[T]]
  val sumMatrix: Quadratic[Int] => Int = map(sum) andThen sum

  // Map Reduce Test Drive
  case class Student(name: String, age: Int, score: Int)

  val students: QList[Student] = cons(Student("Alice", 18, 90))(
    cons(Student("Alex", 20, 110))(
      cons(Student("Bob", 21, 120))(
        cons(Student("Belly", 22, 130))(QNil))))

  object Average {
    def empty: Average = Empty

    def accumulate: Int => Average => Average = x => {
      case Empty => NonEmpty(x, 1)
      case NonEmpty(t, q) => NonEmpty(t + x, q + 1)
    }
  }

  sealed trait Average {
    def value: Option[Int]
  }

  case object Empty extends Average {
    override def value: Option[Int] = None
  }

  case class NonEmpty(total: Int, quantity: Int) extends Average {
    require(quantity > 0)

    override def value: Option[Int] = Some(total / quantity)
  }

  import Average._

  val average: QList[Int] => Option[Int] = xs => foldr[Int, Average](empty)(accumulate)(xs).value

  def average[A](f: A => Int): QList[A] => Option[Int] = map(f) andThen average

  println(average[Student](_.age)(students))
  println(average[Student](_.score)(students))
}

class Node[+T](x: T, xs: => QList[Node[T]]) {
  def label: T = x

  def subtrees: QList[Node[T]] = xs

  override def toString: String = s"Node(${x}, ?)"
}

object Node {
  def apply[T](label: T, subtrees: => QList[Node[T]]): Node[T] = new Node(label, subtrees)

  def unapply[T](node: Node[T]): Option[(T, QList[Node[T]])] = Some((node.label, node.subtrees))

  def reduceTree[LABEL, Y, YS](f: LABEL => YS => Y)(g: Y => YS => YS)(zero: YS): Node[LABEL] => Y = {
    case Node(label, subtrees: QList[Node[LABEL]]) =>
      f(label)(reduceTrees[LABEL, Y, YS](f)(g)(zero)(subtrees))
  }

  private def reduceTrees[LABEL, Y, YS](f: LABEL => YS => Y)(g: Y => YS => YS)(zero: YS): QList[Node[LABEL]] => YS = {
    case QNil => zero
    case QCons(subtree, rest) =>
      g(reduceTree(f)(g)(zero)(subtree))(reduceTrees(f)(g)(zero)(rest))
  }

  def mapTree[X, Y](f: X => Y): Node[X] => Node[Y] = reduceTree[X, Node[Y], QList[Node[Y]]](x => ys => Node(f(x), ys))(cons)(QNil)

  def labels[A]: Node[A] => QList[A] = reduceTree[A, QList[A], QList[A]](cons)(append)(QNil)

  def repeatTree[A](f: A => QList[A])(a: A): Node[A] = Node(a, map(repeatTree[A](f))(f(a)))

  def prune[A](n: Int): Node[A] => Node[A] = {
    case Node(a: A, subtrees: QList[Node[A]]) =>
      if (n <= 0) Node(a, QNil)
      else Node(a, map[Node[A], Node[A]](prune(n - 1))(subtrees))
  }
}

object HigherOderFunctionTreeDemo extends App {
  val tree: Node[Int] = Node(1,
    QCons(Node(2, QNil),
      QCons(Node(3, QCons(Node(4, QNil), QNil)), QNil)))

  val add: Int => Int => Int = x => y => x + y
  val sumTree: Node[Int] => Int = Node.reduceTree(add)(add)(0)
  println(sumTree(tree))
  mapTree[Int, String](_.toString)(tree)
  println(mapTree[Int, Int](_ * 2)(tree))
  println(labels(tree))

  import HigherOrderFunctionListDemo.range

  println(repeatTree(range(10))(1))
}

object TicTacToeGame extends App {
  type Position = (Int, Int)

  sealed trait Piece

  case object X extends Piece

  case object O extends Piece

  object ChessBoard {
    def empty: ChessBoard = ChessBoard(
      Array(
        Array(None, None, None),
        Array(None, None, None),
        Array(None, None, None)
      )
    )

    type Position = (Int, Int)
    type Solution = Set[Position]

    object Direction {
      def all: List[Direction] = Down :: Right :: RightDown :: LeftDown :: Nil
    }

    sealed trait Direction

    object Down extends Direction

    object Right extends Direction

    object RightDown extends Direction

    object LeftDown extends Direction

    sealed trait History {
      def length: Int

      def taken: Set[Position]
    }

    object Empty extends History {
      override def length = 0

      override def taken = Set.empty
    }

    case class NonEmpty(direction: Direction, taken: Set[Position], length: Int) extends History

    def nextPosition(position: (Int, Int), direction: Direction) = direction match {
      case Right => (position._1, position._2 + 1)
      case Down => (position._1 + 1, position._2)
      case RightDown => (position._1 + 1, position._2 + 1)
      case LeftDown => (position._1 + 1, position._2 - 1)
    }

    def allWinningSolutions(rows: Int, columns: Int, win: Int): Set[Solution] = {
      def goEachDirection(solutions: Set[Set[(Int, Int)]], direction: Direction, rows: Int, columns: Int, win: Int, left: Int, history: History, position: (Int, Int)): Set[Solution] = {
        val taken = go(solutions, rows, columns, win, left - 1, NonEmpty(direction, history.taken + position, history.length + 1), nextPosition(position, direction))
        go(taken, rows, columns, win, win, Empty, nextPosition(position, direction))
      }

      @tailrec
      def go(acc: Set[Set[Position]], rows: Int, columns: Int, win: Int, left: Int, history: History, position: Position): Set[Solution] = {
        if (left == 0)
          go(acc + history.taken, rows, columns, win, win, Empty, position)
        else if (position._1 >= rows || position._2 >= columns || position._1 < 0 || position._2 < 0) acc
        else history match {
          case Empty =>
            Direction.all.foldLeft(acc)((solutions: Set[Solution], direction: Direction) =>
              goEachDirection(solutions, direction, rows, columns, win, left, history, position))
          case h@NonEmpty(direction, _, _) =>
            goEachDirection(acc, direction, rows, columns, win, left, h, position)
        }
      }

      go(Set.empty, rows, columns, win, win, Empty, (0, 0))
    }

  }

  case class ChessBoard(matrix: Array[Array[Option[Piece]]]) {
    override def toString: String = {
      matrix.indices.map(i => matrix(i)
        .map(_.getOrElse(" "))
        .mkString(" ", " | ", " "))
        .mkString(s"\n${matrix.indices.map(_ => "---").mkString("|")}\n")
    }

    def maybeNextRound: Option[Piece] = {
      val flatten = matrix.flatten
      val Xs = flatten.count(_.contains(X))
      val Os = flatten.count(_.contains(O))
      if (Xs < Os) Some(X)
      else if (Xs > Os) Some(O)
      else if (Xs == Os && Os + Xs < 9) Some(X)
      else None
    }

    def possibleMoves: QList[ChessBoard] =
      if (static == Int.MinValue || static == Int.MaxValue) QNil
      else maybeNextRound.map { piece =>
        map[Position, ChessBoard](p => copy(move(matrix, p, piece)))(emptyPositions)
      }.getOrElse(QNil)

    def static: Int = {
      if (humanWin()) Int.MinValue
      else if (computerWin()) Int.MaxValue
      else computerFuture() - humanFuture()
    }

    private def emptyPositions: QList[(Int, Int)] =
      fromArray(matrix.flatten.zipWithIndex.filter(_._1.isEmpty)
        .map(p => (p._2 / 3, p._2 % 3)))

    private def move(matrix: Array[Array[Option[Piece]]], p: Position, piece: Piece): Array[Array[Option[Piece]]] = {
      val index = ArrayBuffer(0, 1, 2)
      val r: ArrayBuffer[ArrayBuffer[Option[Piece]]] = ArrayBuffer.fill(3)(ArrayBuffer.fill(3)(None))
      for {
        i <- index
        j <- index
      } {
        if ((i, j) == p)
          r(i)(j) = Some(piece)
        else
          r(i)(j) = matrix(i)(j)
      }
      r.map(_.toArray).toArray
    }

    private def humanWin(): Boolean = isWinner(X)

    private def computerWin(): Boolean = isWinner(O)

    private def computerFuture(): Int = futureFor(O)

    private def humanFuture(): Int = futureFor(X)

    private def futureFor(p: Piece): Int = count(pOrNones(p, matrix))(allWinningPossible)

    private def pOrNones(p: Piece, matrix: Array[Array[Option[Piece]]]): QList[Position] => Boolean = positions =>
      allTrue(map[Position, Boolean](pos => pOrNone(p, matrix(pos._1)(pos._2)))(positions))

    private def pOrNone(p: Piece, actualPiece: Option[Piece]): Boolean =
      actualPiece.contains(p) || actualPiece.isEmpty

    private def formatSolution(solution: Set[Position]): QList[Position] = solution.toList.sortWith((p,q) => p._1 < q._1 || p._2 < q._2).foldLeft[QList[Position]](QNil)((xs, x) => cons[Position](x)(xs))

    private lazy val allWinningPossible: QList[QList[Position]] =
          allWinningSolutions(3, 3, 3)
            .map(formatSolution)
            .foldLeft[QList[QList[Position]]](QNil)((xs, x) => cons(x)(xs))
//      QCons(QCons((0, 0), QCons((0, 1), QCons((0, 2), QNil))),
//        QCons(QCons((0, 0), QCons((1, 1), QCons((2, 2), QNil))),
//          QCons(QCons((0, 0), QCons((0, 1), QCons((0, 2), QNil))),
//            QCons(QCons((1, 0), QCons((1, 1), QCons((1, 2), QNil))),
//              QCons(QCons((2, 0), QCons((1, 1), QCons((0, 2), QNil))),
//                QCons(QCons((2, 0), QCons((2, 1), QCons((2, 2), QNil))),
//                  QCons(QCons((0, 1), QCons((1, 1), QCons((2, 1), QNil))),
//                    QCons(QCons((0, 2), QCons((1, 2), QCons((2, 2), QNil))),
//                      QNil))))))))

    private def isWinner(p: Piece): Boolean =
      anyTrue(map(matched(p, matrix))(allWinningPossible))

    private def matched(p: Piece, matrix: Array[Array[Option[Piece]]]): QList[Position] => Boolean = positions =>
      allTrue(map(contains(p, matrix))(positions))

    private def contains(p: Piece, matrix: Array[Array[Option[Piece]]]): Position => Boolean = {
      case (x, y) => matrix(x)(y).contains(p)
    }
  }

  type EvaluatedBoard = (ChessBoard, Int)

  def evaluate: ChessBoard => EvaluatedBoard = gameTree andThen prune(8) andThen mapTree(static) andThen maximize

  // 构造博弈树
  private def gameTree: ChessBoard => Node[ChessBoard] = repeatTree[ChessBoard](moves)

  private def moves: ChessBoard => QList[ChessBoard] = _.possibleMoves

  // 评价博弈树
  private def static: ChessBoard => EvaluatedBoard = c => (c, c.static)

  // 寻求最优解
  private def maximize: Node[EvaluatedBoard] => EvaluatedBoard = {
    case Node((c, n), QNil) => (c, n)
    case Node(_, sub) => max(map(minimize)(sub))
  }

  private def minimize: Node[EvaluatedBoard] => EvaluatedBoard = {
    case Node(n, QNil) => n
    case Node(_, sub) => min(map(maximize)(sub))
  }

  private def max: QList[EvaluatedBoard] => EvaluatedBoard = reduce((ChessBoard.empty, Int.MinValue))(x => y => if (x._2 >= y._2) x else y)

  private def min: QList[EvaluatedBoard] => EvaluatedBoard = reduce((ChessBoard.empty, Int.MaxValue))(x => y => if (x._2 <= y._2) x else y)

  // while finding max, omit the minor branch

  private def maximizeOmit: Node[EvaluatedBoard] => EvaluatedBoard = max compose maximizeGrand
  private def minimizeOmit: Node[EvaluatedBoard] => EvaluatedBoard = min compose minimizeGrand
  private def maximizeGrand: Node[EvaluatedBoard] => QList[EvaluatedBoard] = {
    case Node(n, QNil) => QCons(n, QNil)
    case Node(_, l)    => mapmin( map(minimizeGrand)(l) )
  }
  private def minimizeGrand: Node[EvaluatedBoard] => QList[EvaluatedBoard] = {
    case Node(n, QNil) => QCons(n, QNil)
    case Node(_, l) => mapmax( map(maximizeGrand)(l))
  }
  private def mapmin: QList[QList[EvaluatedBoard]] => QList[EvaluatedBoard] = {
    case QCons(nums, rest) =>
      val minboard = min(nums)
      cons(minboard)(omitMinor(minboard)(rest))
  }
  private def mapmax: QList[QList[EvaluatedBoard]] => QList[EvaluatedBoard] = {
    case QCons(nums, rest) =>
      val maximum = max(nums)
      cons(maximum)(omitMajor(maximum)(rest))
  }
  private def omitMinor(pot: EvaluatedBoard): QList[QList[EvaluatedBoard]] => QList[EvaluatedBoard] = {
    case QNil => QNil
    case QCons(nums, rest) if minleq(nums)(pot) => omitMinor(pot)(rest)
    case QCons(nums, rest) =>
      val minboard = min(nums)
      cons(minboard)(omitMinor(minboard)(rest))
  }
  private def omitMajor(pot: EvaluatedBoard): QList[QList[EvaluatedBoard]] => QList[EvaluatedBoard] = {
    case QNil => QNil
    case QCons(nums, rest) =>
      if (maxleq(nums)(pot)) omitMajor(pot)(rest)
      else {
        val maximum = max(nums)
        cons(maximum)(omitMajor(maximum)(rest))
      }
  }

  private def minleq(nums: QList[EvaluatedBoard])(pot: EvaluatedBoard): Boolean = nums match {
    case QNil => false
    case QCons(num, rest) =>
      if(num._2 <= pot._2) true
      else minleq(rest)(pot)
  }

  private def maxleq(nums: QList[EvaluatedBoard])(pot: EvaluatedBoard): Boolean = nums match {
    case QNil => false
    case QCons(num, rest) =>
      if(num._2 >= pot._2) true
      else maxleq(rest)(pot)
  }
  def evaluate3: ChessBoard => EvaluatedBoard =
    gameTree andThen prune(8) andThen mapTree(static) andThen maximizeGrand andThen max

  // highfirst
  private def highfirst: Node[EvaluatedBoard] => Node[EvaluatedBoard] = {
    case Node(n, sub) => Node(n, sort(higher)(map(lowfirst)(sub)))
  }
  private def lowfirst: Node[EvaluatedBoard] => Node[EvaluatedBoard] = {
    case Node(n, sub) => Node(n, sort(not(higher))(map(highfirst)(sub)))
  }
  private def higher: (Node[EvaluatedBoard],Node[EvaluatedBoard]) => Boolean =
    (x, y) => x.label._2 > y.label._2
  private def not(f: (Node[EvaluatedBoard],Node[EvaluatedBoard]) => Boolean): (Node[EvaluatedBoard],Node[EvaluatedBoard]) => Boolean =
    (x, y) => !f(x, y)
  private def sort[A](f:(A, A) => Boolean): QList[A] => QList[A] =
    xs => fromScala(toScala(xs).sortWith(f))

  def evaluate4: ChessBoard => EvaluatedBoard =
    gameTree andThen prune(8) andThen mapTree(static) andThen highfirst andThen maximizeGrand andThen max

  // take 3
  import Node._
  def taketree[A](n: Int): Node[A] => Node[A] = reduceTree[A, Node[A], QList[Node[A]]](nodett[A](n))(cons)(QNil)
  def nodett[A](n: Int): A => QList[Node[A]] => Node[A] = label => sub => Node(label, sub.take(n))

  def evaluate5: ChessBoard => EvaluatedBoard =
    gameTree andThen prune(8) andThen mapTree(static) andThen taketree(3) andThen maximizeGrand andThen max

  // Test Drives
  val board1 = ChessBoard(Array(
    Array(None, None, None),
    Array(None, Some(O), None),
    Array(Some(X), Some(O), Some(X))
  ))

  val board2 = ChessBoard(Array(
    Array(None, None, None),
    Array(None, Some(X), None),
    Array(Some(O), Some(X), Some(O))
  ))

  val board3 = ChessBoard(Array(
    Array(None, None, None),
    Array(None, Some(O), None),
    Array(Some(X), Some(X), None)
  ))
  val board4 = ChessBoard(Array(
    Array(None, None, None),
    Array(None, Some(O), None),
    Array(Some(X), None, None)
  ))

  println(
    foldr(" \n" * 8)(horizontalLayout)(map(play)(QCons(board1, QCons(board2, QCons(board3, QCons(board4, QNil))))))
  )

  private def play(board: ChessBoard): String = {
    val evaluated: (ChessBoard, Int) = evaluate5(board)
    val chessLayout: String = horizontalLayout(board.toString)(evaluated._1.toString)
    val chessResult: String = evaluated._2 match {
      case 0 => "=" * 26 + " 平局 "
      case Int.MaxValue => "=" * 20 + " 计算机获胜 "
      case Int.MinValue => "=" * 22 + " 人类获胜 "
    }
    chessLayout + "\n" + chessResult
  }

  private def horizontalLayout: String => String => String = one => other => {
    val space = " "
    one.split("\n").zip(other.split("\n"))
      .map(z => s"${z._1}${space * 4}|${space * 4}${z._2}")
      .mkString("\n")
  }

  def initializeArrayBuffer(r: Int, c: Int): ArrayBuffer[ArrayBuffer[Option[Piece]]] = ArrayBuffer.fill(r, c)(None)

  val r, c = 4
  val renderSolution: Solution => ChessBoard = solution => {
    val chess = initializeArrayBuffer(r, c)
    for {
      position <- solution
    } chess(position._1)(position._2) = Some(O)
    new ChessBoard(chess.map(_.toArray).toArray)
  }

  def allChessboards: Set[ChessBoard] =
    allWinningSolutions(r, c, 3)
      .map(renderSolution)

  allChessboards.foreach{ board =>
    println
    println(board)
    println
  }
}