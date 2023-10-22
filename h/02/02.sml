datatype number = Zero | Succ of number | Pred of number;

fun simp Zero = Zero
    | simp (Pred x) = 
        (case simp x of
                Succ y => y
            | y => Pred y)
    | simp (Succ x) = 
        (case simp x of
                Pred y => y
            | y => Succ y)

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number =
        case a of
            Zero => Zero
        | Succ a => Pred (neg(a))
        | Pred a => Succ (neg(a));

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number = 
        case a of
            Zero => Zero 
        | Succ a => Succ (add(a, b))
        | Pred a => Succ (add(a, b));

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) : order = 
        let val simp_a = simp(a)
            val simp_b = simp(b)
        in
            case (simp_a, simp_b) of
                (Zero, Zero) => EQUAL
            | (Succ x_a, Succ x_b) => comp(x_a, x_b)
            | (Pred x_a, Pred x_b) => comp(x_a, x_b)
            | (Succ x_a, _) => GREATER
            | (_, Pred x_b) => GREATER
            | _ => LESS
        end;
