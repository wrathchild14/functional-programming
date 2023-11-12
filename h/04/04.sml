fun reduce f z [] = z
  | reduce f z (x::xs) = reduce f (f z x) xs;

fun squares [] = []
  | squares xs = List.map (fn x => x * x) xs;

fun onlyEven [] = []
  | onlyEven xs = List.filter (fn x => x mod 2 = 0) xs;

fun bestString f [] = ""
  | bestString f (x::xs) = List.foldl (fn (x, y) => if f(x, y) then x else y) x xs;

fun largestString xs = bestString (fn (x, y) => String.compare(x, y) = GREATER) xs;

fun longestString xs = bestString (fn (x, y) => size x > size y) xs;
