fun tip a (b, c) =
 case (b, a) of
        (_, (SOME c)::x) => x
        | _ => tl a

fun add (x:int, y:int) = x + y;
val addCurried = fn x => fn y => add(x, y);

fun x {a=b, c=d} h =
        case (b, d) of
                (SOME e, f::g) => e andalso f andalso (x {a=b, c=g} h)
            | (NONE, f::g) => f andalso (x {a=b, c=g} h)
            | _ => h

fun factorial n = 
        let 
                fun helper acc 0 = acc
                    | helper acc n = helper (acc * n) (n - 1)
        in
                helper 1 n
        end;

fun factorial 0 = 1
    | factorial n = n * factorial (n - 1);

datatype datum = A of int | B of int list;

fun check [] = true
    | check (A _ :: B xs :: rest) = 
            (List.all (fn x => x = 3 orelse x = 4) xs) andalso
            (List.last xs = 4) andalso
            check rest
    | check (A _ :: []) = true
    | check _ = false;

check [A 1, B [3,4], A 3];
check [A 9, B [3,4], A 4, B [4,3,4,3,4], A 2, B [4]];
check [B [3,4], A 1, B [4,3]];
check [A 1, B [3,4,3]];

fun f x y z = List.foldl (fn (g1::g2::r, y) => SOME(g2+(valOf y)))

datatype ('a, 'b) node = Node of (('b, 'a) node * ('a) * ('b, 'a) node) |
fin;

fun height drevo =
  case drevo of
    fin => 1
  | Node (l, _, d) => 1 + Int.max (height l, height d);


fun F2bad a b =
    let
        fun pomozna(a, b, acc) =
            if a = 0
            then acc
            else pomozna(a-1, b, acc+b)
    in
        pomozna(a, b, 0)
    end;
fun G2bad a b =
    let
        fun pomozna(a, b, acc) =
            if b=0
            then acc
            else let val r = G2bad a (b-1) in F2bad a r end;
    in
        pomozna(a, b, 1)
    end;
(* Equal but no accumulator *)
fun F2 a b = 
    let 
    fun helper a b = 
        if a=0
        then 0 
        else
        b + helper (a-1) b
    in helper a b
    end;

fun G2 a b = 
    let 
    fun helper a b = if b=0 then 1 else
        F2 a (helper a (b-1))
    in helper a b end;

infix o;
(* o : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) *)
fun f o g = fn x => f (g x);

fun map f [] = []
    | map f (x::xs) = (f x)::map f xs;

fun filter p [] = []
    | filter p (x::xs) = if (p x) then x::filter p xs else filter p xs;

fun foldl f z [] = z
    | foldl f z (x::xs) = foldl f (f (x, z)) xs;

fun foldr f z [] = z
    | foldr f z (x::xs) = f(x, foldr f z xs);

val double : int -> int = fn n => 2 * n;
fun square (f : unit -> int) : int = f () * f ();

datatype ('a, 'b) set = elem of 'a list * ('a, 'b) set | empty of 'b
fun fun1 (x, y) [z] =
 case y of
 elem ([nil], empty z) => x
 | _ => SOME z


val test_zapis = {1="Test", 2="Test2"};

fun split lst = 
    List.foldl (fn (x, (l, r)) => if length l <= length r then (l@[x], r) else (l, r@[x])) ([], []) lst;

val test_split = split [1,2,3,4,5,6,7,8,9,10];
val test_izpit_split = split [2,3,2,5,6,8,4,5];

fun testna a {a=b, b=c} = 
    List.map (fn x => a + (valOf b) - c(a));

fun insert(x, []) = [x]
    | insert(x, y::ys) =
            if x <= y then x::y::ys
            else y::insert(x, ys);

fun sort([]) = []
    | sort(x::xs) = insert(x, sort(xs));

