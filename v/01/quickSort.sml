fun implies (z : bool * bool) : bool = 
    not (#1 z) orelse (#2 z);

fun partition (x: int, xs : int list) : int list * int list =
    let 
      fun obrni (xs : int list, ys : int list) : int list =
          if null xs then ys
          else obrni(tl xs, hd xs::ys);

      fun partition (xs : int list, l : int list, r : int list) : int list * int list = 
          if null xs
          then (obrni(l, []), obrni(r, []))
          else if hd xs < x
            then partition (tl xs, hd xs::l, r )
            else partition(tl xs, l, hd xs::r)
    in 
      partition (xs, [], [])
    end;

fun quickSelect (k : int, xs : int list) : int option =
    if k < 0 orelse null xs then NONE else 
      let
        val a = hd xs
        val p : int list * int list = partition (a, xs)
        val l : int list = #1 p
        val r : int list = #2 p 
    
        fun len (xs : int list, i : int) : int =
            if null xs then i
            else len (tl xs, i + 1)

        val len_l : int = len (l, 0)
      in
        if len_l = k then SOME a
        else if len_l > k then quickSelect (k, l)
          else quickSelect (k - len_l - 1, r)
      end;

quickSelect(0, [10, 4, 5, 8, 6, 6, 6, 11, 26]);
quickSelect(4, [10, 4, 5, 8, 6, 6, 6, 11, 26]);
quickSelect(6, [10, 4, 5, 8, 6, 6, 6, 11, 26]);
quickSelect(8, [10, 4, 5, 8, 6, 6, 6, 11, 26]);
