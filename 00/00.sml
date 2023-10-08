(* Vrne naslednika števila `n`. *)
fun next (n : int) : int = n + 1;

(* Vrne vsoto števil `a` in `b`. *)
fun add (a : int, b : int) : int = a + b;

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) : bool = a andalso b orelse c orelse b andalso c;

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real)
   namig: uporabi Real.max in Real.min *)
fun median(a: real, b: real, c: real): real =
            if a <= b andalso b <= c orelse c <= b andalso b <= a
            then b
            else if b <= a andalso a <= c orelse c <= a andalso a <= b
                  then a
                  else c;

(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a: int, b : int, c : int) : bool = a + b > c andalso a + c > b andalso b + c > a;

(* Vrne rezultat potenciranja - `base` ^ `exponent` (`exponent` > 0)
   pow (5, 3) = 125 *)
fun pow (base : LargeInt.int, exponent : LargeInt.int) : LargeInt.int = if exponent = 1
            then base
            else base * pow(base, exponent - 1);
