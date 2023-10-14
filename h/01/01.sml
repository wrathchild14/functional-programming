(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n : int) : int = if n=0 then 1 else if n = 1 then 1 else n * factorial(n - 1);

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x : int, n : int) : int = if n = 0 then 1 else x * power(x, n - 1);

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, b : int) : int = if b <> 0 then gcd(b, a mod b) else a;

(*  Vrne dolžino seznama. *)
fun len (xs : int list) : int = if null xs then 0 else 1 + len(tl xs);

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last (xs : int list) : int option =
    if null xs then NONE 
    else if null (tl xs)
      then SOME(hd xs) 
      else
        last(tl xs);

(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth (xs : int list, n : int) : int option = 
    if n < 0 orelse n > len(xs) - 1 then
      NONE 
    else 
      if n=0 then SOME(hd xs) else nth(tl xs, n - 1);

(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, n : int, x : int) : int list = 
    if n < 0 orelse n > len(xs) - 1 then
      []
    else
      if n=0 then x :: xs else hd xs :: insert(tl xs, n - 1, x);

(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete (xs : int list, x : int) : int list =
    if null xs then 
      []
    else if hd xs = x then 
        delete(tl xs, x)
      else 
        hd xs::delete(tl xs, x)

(*  Vrne obrnjen seznam. V pomoč si lahko spišete še funkcijo append, ki doda na konec seznama. *)
fun reverse (xs : int list) : int list =
    let fun append (xs : int list, x : int) : int list = 
          if null xs then [x]
          else (hd xs)::append(tl xs, x)
    in
      if null xs then []
      else append(reverse(tl xs), hd xs)
    end;

(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun palindrome (xs : int list) : bool = xs = reverse(xs);