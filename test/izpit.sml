(* [2,3,2,5,6,8,4,5] vrne terko ([2,2,6,4],[3,5,8,5]) *)

fun splitList lst = 
  let
    fun f (evens, odds) x = 
      if (length x) mod 2 = 0 then (x::evens, odds)
      else (evens, x::odds)
  in
    foldl f ([], []) lst
  end;
