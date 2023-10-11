val _ = print "~~~~~~~~ factorial ~~~~~~~~\n";
val test_type: int -> int = factorial;
val test = factorial (5) = 120;

val _ = print "~~~~~~~~ power ~~~~~~~~\n";
val test_type: int * int -> int = power;
val test = power (2, 0) = 1;

val _ = print "~~~~~~~~ gcd ~~~~~~~~\n";
val test_type: int * int -> int = gcd;
val test = gcd (366, 60) = 6;

val _ = print "~~~~~~~~ len ~~~~~~~~\n";
val test_type: int list -> int = len;
val test = len ([1,2,3,4,5]) = 5;

val _ = print "~~~~~~~~ last ~~~~~~~~\n";
val test_type: int list -> int option = last;
val test = last ([1,2,3,4,5]) = SOME(5);