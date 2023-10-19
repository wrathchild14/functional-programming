type student = {absolvent:bool, ime:string, ocene:(string * int) list, starost : int}

val x = {ime="Dejan", starost=21, absolvent=false, ocene=[("angl", 8), ("ars", 10)]}

fun izpis_studenta (zapis : {absolvent:bool, ime:string, ocene: (string * int) list, starost:int}) =
    (#ime zapis) ^ " je star " ^ Int.toString(#starost zapis) ^ " let.";

(* izpis_studenta(x); *)

(* drugi primer - artikli v trgovini *)
type artikel = string * int
val a1: string * int = ("kruh", 1)
val a2: artikel = ("mleko", 3)
val a3 = ("argentinski steak", 80)