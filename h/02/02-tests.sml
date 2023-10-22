val num = Succ(Pred(Succ(Pred(Pred(Succ(Succ(Zero)))))));
val num1 = Succ(Succ(Zero));
val num2 = Pred(Pred(Zero));

val neg_test = neg(num);
val simp_test = simp(num);

val add_test = add(num1, num2);
val comp_test = comp(num1, num2);


val tree = Node(51, Leaf ~1, Node (2, Node (5, Leaf 4, Leaf 12), Leaf 34))

val contains_test = contains(tree, 2);
val count_leaves_test = countLeaves(tree);
val count_branches_test = countBranches(tree);
val height_test = height(tree);