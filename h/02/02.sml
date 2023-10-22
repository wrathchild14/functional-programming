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

datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) : bool = 
        case tree of
            Leaf l => l = x
        | Node (t, l, r) => t = x orelse contains(l, x) orelse contains(r, x);

(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int = 
        case tree of
            Leaf l => 1
        | Node (t, l, r) => countLeaves(l) + countLeaves(r)

(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) : int =
        case tree of
            Leaf l => 0
        | Node(t, l, r) => 2 + countBranches(l) + countBranches(r);

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int = 
        case tree of
            Leaf l => 1
        | Node(t, l, r) => 1 + Int.max(height(l), height(r));

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
(* fun toList (tree : tree) : int list = *)

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
(* fun isBalanced (tree : tree) : bool *)

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
(* fun isBST (tree : tree) : bool *)
