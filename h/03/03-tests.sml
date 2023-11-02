val list1 = [1, 2, 3, 4];
val list2 = ["a", "b", "c"];
val zippedList = zip(list1, list2);

val (list1, list2) = unzip(zippedList);

val subtract_result = subtract (Succ (Succ (Succ One)), Succ One);  (* Succ (Succ One) *)

val _ = print "~~~~~~~~ fun tests ~~~~~~~~\n";
val big_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
val is_even_fn = fn x => x mod 2 = 0;
val any_result = any(is_even_fn, big_list);
val map_result = map(is_even_fn, big_list);
val filter_result = filter(is_even_fn, big_list);

val sum = fn (x, y) => x + y;
val foldl_result = foldl(sum, 0, big_list);


(* izpis daljših izrazov v interpreterju *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;

(* izpis drevesa po nivojih *)
fun showTree (toString : 'a -> string, t : 'a bstree) =
    let fun strign_of_avltree_level (lvl, t) = case t of  
            lf => if lvl = 0 then "nil" else "   "
          |   br (l, n, r) =>
            let val make_space = String.map (fn _ => #" ")
              val sn = toString n
              val sl = strign_of_avltree_level (lvl, l)
              val sr = strign_of_avltree_level (lvl, r)
            in if height t = lvl
              then make_space sl ^ sn ^ make_space sr
              else sl ^ make_space sn ^ sr
            end
      fun print_levels lvl =
          if lvl >= 0
          then (print (Int.toString lvl ^ ": " ^ strign_of_avltree_level (lvl, t) ^ "\n");
              print_levels (lvl - 1))
          else ()
    in  print_levels (height t)
    end;

(* primeri vstavljanja elementov v AVL drevo *)
fun avlInt (t, i) = avl (Int.compare, t, i);
fun showTreeInt t = showTree(Int.toString, t);

val tr = lf : int bstree;
val _ = showTreeInt tr;
val tr = avlInt (tr, 1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 2);
val _ = showTreeInt tr;
val tr = avlInt (tr, 3);
val _ = showTreeInt tr;
val tr = avlInt (tr, 4);
val _ = showTreeInt tr;
val tr = avlInt (tr, 5);
val _ = showTreeInt tr;
val tr = avlInt (tr, 6);
val _ = showTreeInt tr;
val tr = avlInt (tr, 7);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~4);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~3);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~2);
val _ = showTreeInt tr;
val tr = avlInt (tr, ~1);
val _ = showTreeInt tr;
val tr = avlInt (tr, 0);
val _ = showTreeInt tr;

val from0to13 = fold (fn (z, x) => avl (Int.compare, z, x), lf, List.tabulate (14, fn i => i));