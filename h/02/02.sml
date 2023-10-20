datatype number = Zero | Succ of number | Pred of number;

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number =
    case a of
       Zero => Zero
     | Succ a => Succ (neg a)
     | Pred a => Pred (neg a);

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
(* fun add (a : number, b : number) : number *)

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
(* fun comp (a : number, b : number) : order *)
