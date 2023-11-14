(* val sez = ref []; *)
(* sez := !sez @ [5]; *)
(* sez := !sez @ [true]; *)

fun compose(f,g) =
  fn x => f(g x);
(* 1. f: 'a->'b; g:'c->'d;
compose ('a -> 'b) * ('c->'a) -> ('c->'b)
2. x: 'c; (g x): 'd; (g x): 'a; 'a='d;
'e = 'c -> 'b *)