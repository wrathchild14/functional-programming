val num = Succ(Pred(Succ(Pred(Pred(Succ(Succ(Zero)))))));
val num1 = Succ(Succ(Zero));
val num2 = Pred(Pred(Zero));

val neg_test = neg(num);
val simp_test = simp(num);

val add_test = add(num1, num2);
val comp_test = comp(num1, num2);


val tree1 = Node(51, Leaf ~1, Node (2, Node (5, Leaf 4, Leaf 12), Leaf 34))

val tree2 = Node(1, Node(2, Leaf 4, Leaf 5), Node(3, Leaf 6, Leaf 7));
val tree3 = Node(8, Node(3, Leaf 1, Node(6, Leaf 4, Leaf 7)), Node(10, Leaf 0, Node(14, Leaf 13, Leaf 0)))

val contains_test = contains(tree1, 2);
val count_leaves_test = countLeaves(tree1);
val count_branches_test = countBranches(tree1);
val height_test = height(tree1);

val tree_list1 = toList(tree1);
val tree_list2 = toList(tree2);
val tree_list3 = toList(tree3);

val is_balanced1 = isBalanced(tree1);
val is_balanced2 = isBalanced(tree2);

val is_bst1 = isBST(tree1);
val is_bst2 = isBST(tree2);
val is_bst3 = isBST(tree3);

val tree1 = Node (3, Node (2, Leaf 1, Leaf 4), Node (5, Leaf 4, Leaf 6));
val tree2 = Node (3, Node (4, Leaf 1, Leaf 2), Node (5, Leaf 6, Leaf 7));

val isBST1 = isBST tree1;  (* Should return true *)
val isBST2 = isBST tree2;  (* Should return false *)

