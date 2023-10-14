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
val test = len ([1, 2, 3, 4, 5]) = 5;

val _ = print "~~~~~~~~ last ~~~~~~~~\n";
val test_type: int list -> int option = last;
val test = last ([1, 2, 3, 4, 5]) = SOME(5);

val _ = print "~~~~~~~~ nth ~~~~~~~~\n";
val test_type: int list * int -> int option = nth;
val test = nth ([1, 2, 3, 4, 5], 2) = SOME(3);

val _ = print "~~~~~~~~ insert ~~~~~~~~\n";
val test_type: int list * int * int -> int list = insert;
val test1 = insert ([1, 2, 3, 4, 5], 2, 99) = [1, 2, 99, 3, 4, 5];
val test2 = insert ([1, 2, 3, 4, 5], 0, 99) = [99, 1, 2, 3, 4, 5];

val _ = print "~~~~~~~~ delete ~~~~~~~~\n";
val test_type: int list * int -> int list = delete;
val test1 = delete ([1, 2, 3, 4, 5], 4) = [1, 2, 3, 5];
val test2 = delete ([1, 2, 3, 4, 5], 1) = [2, 3, 4, 5];
val test3 = delete ([1, 2, 3, 4, 5], 5) = [1, 2, 3, 4];

val _ = print "~~~~~~~~ reverse ~~~~~~~~\n";
val test_type: int list -> int list = reverse;
val test = reverse ([1, 2, 3, 4, 5]) = [5, 4, 3, 2, 1];

val _ = print "~~~~~~~~ palindrome ~~~~~~~~\n";
val test_type: int list -> bool = palindrome;
val test1 = palindrome ([1, 2, 3, 2, 1]) = true;
val test2 = palindrome ([1, 2, 3, 4, 5]) = false;
val test3 = palindrome ([1, 1, 1, 1, 1]) = true;
val test4 = palindrome ([1, 2, 1, 2, 1]) = true;
val test5 = palindrome ([1, 2, 2, 1]) = true;
