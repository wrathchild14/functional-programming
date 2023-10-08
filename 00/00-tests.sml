(* use "00.sml"; *)
val _ = print "~~~~~~~~ next ~~~~~~~~\n";
val test_type: int -> int = next;
val test = next (~1) = 0;

val _ = print "~~~~~~~~ add ~~~~~~~~\n";
val test_type: int * int -> int = add;
val test = add (~1, 1) = 0;

val _ = print "~~~~~~~~ majority ~~~~~~~~\n";
val test_type: bool * bool * bool -> bool = majority;
val test = majority (false, false, true) = false;

val _ = print "~~~~~~~~ median ~~~~~~~~\n";
val test_type: real * real * real -> real = median;
val == = Real.==;
infix ==;
val test = median (1.1, ~1.0, 1.0) == 1.0;

val _ = print "~~~~~~~~ triangle ~~~~~~~~\n";
val test_type: int * int * int -> bool = triangle;
val test = triangle (~1, ~1, ~1) = false;

val _ = print "~~~~~~~~ pow ~~~~~~~~\n";
val test_type: LargeInt.int * LargeInt.int -> LargeInt.int = pow;
val test = pow (5, 3) = 125;

val _ = print "~~~~~~~~ log ~~~~~~~~\n";
val test_type: int * int -> int = log;
val test = log (3, 12345) = 8;
