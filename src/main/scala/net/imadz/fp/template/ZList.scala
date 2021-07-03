package net.imadz.fp.template


// paradigm vs pattern
// Session 1: Higher Order Functions with List and List Combinator Functions
object RList {
  // 1. Define List[+T] ADT as Nil | Cons
  // 2. Implement sum, product, anyTrue, allTrue
  // 3. Extract foldr combinator function
  // 4. Express sum, product, anyTrue, and allTrue with foldr function
  // 5. Define append combinator function
  // 6. Define doubleAll function
  // 7. Extract map combinator function
  // 8. Define tail recursive version foldr to fix stack overflow error
  // 9. Define lazy evaluation version List

}

object RListDemo extends App {

  import RList._

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