(* implicni vektor 1 *)
fun reduce v m = map (fn x :: u => Vec.sub u (Vec.scale x v) | _ => raise Empty) m

fun pivot ((v as x :: _ ):: m) = 
  (case R.inv x of 
      SOME x' => SOME (Vec.scale x' v :: m)
    | NONE =>
        case m of
            ((u as y :: _) :: m') => 
              let val (g, s, t) = R.xGCD (x, y) in
              case pivot (Vec.add (Vec.scale s v) (Vec.scale t u) :: m') of
                  SOME (w :: m'') => SOME (w :: u :: v :: m'')
                | _ => NONE
              end
          | _ => NONE)
  | pivot _ = NONE

(* Model List je odpren: List.filter, List.exists ... *)
fun gauss (above, []) = SOME above
  | gauss (above, below) = 
    case pivot below of 
      SOME ((_ :: v) :: m) => gauss (reduce v above @ [v], 
        filter (exists (fn x => x <> R.zero)) (reduce v m)) (* na m, ker smo spremenili below 
          pustimo vsi elementi ki nimajo 0*)
    | _ => NONE