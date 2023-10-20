datatype number = Zero | Succ of number | Pred of number

fun simp Zero = Zero
  | simp (Pred x) = 
    (case simp x of
        Succ y => y
      | y => Pred y)
  | simp (Succ x) = 
    (case simp x of
        Pred y => y
      | y => Succ y)

datatype tree = Node of int * tree * tree | Leaf of int

fun min (Leaf x) = x
  | min (Node (x, l, r)) = Int.min(x, Int.min(min l, min r))

val d = Node(51, Leaf ~1, Node (2, Node (5, Leaf 4, Leaf 12), Leaf 34))
val minimum = min(d)
