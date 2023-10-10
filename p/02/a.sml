(* missing *)

(* povprecje z lokalno okolje *)
fun povprecje (sez: int list) =
        let
            fun stevilo_el(sez: int list) =
                    if null sez
                    then 0
                    else 1 + stevilo_el(tl sez)
            fun vsota_el(sez: int list) = 
                    if null sez
                    then 0
                    else hd sez + vsota_el(tl sez)
            val vsota_el = Real.fromInt(vsota_el(sez))
            val st_el = Real.fromInt(stevilo_el(sez))
        in
            vsota_el/st_el
        end

fun najvecji3 (sez: int list) =
        if null sez
        then NONE
        else let val max_rep = najvecji3(tl sez) (* podatkovni tip max_rep je int option ! *)
            in
                if isSome(max_rep) andalso valOf(max_rep) > hd sez
                then max_rep
                else SOME (hd sez)
            end

(* val test = povprecje([5,3,2,3,1]); *)
(* val test1 = najvecji3([]); *)
(* val test2 = najvecji3([1, 5, 3, 34, 32532, 6]); *)