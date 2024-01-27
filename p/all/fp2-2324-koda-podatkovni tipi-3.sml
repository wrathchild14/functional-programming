Control.Print.printDepth := 100;


(***************************************************************)
(************************* 1. TERKE  ***************************)
(***************************************************************)

(* sestej stevili, podani v terki *)
fun vsota (stevili: int*int) =
    (#1 stevili) + (#2 stevili)

(* obrni elementa terke-para *)	
fun obrni (stevili: int*int) =
    (#2 stevili, #1 stevili)

(* prepleti dve trimestni terki *)
fun prepleti (terka1: int*int*int, terka2: int*int*int) =
    (#1 terka1, #1 terka2, #2 terka1, #2 terka2, #3 terka1, #3 terka2)

(* sortiraj par stevil v terki po velikosti *)
fun sortiraj_par (terka: int*int) =
    if #1 terka < #2 terka
    then terka
    else (#2 terka, #1 terka)



(***************************************************************)
(************************* 2. SEZNAMI **************************)
(***************************************************************)

(* stevilo elementov v seznamu *)
fun stevilo_el(sez: int list) =
    if null sez
    then 0
    else 1 + stevilo_el(tl sez)

(* vsota elementov v seznamu *)
fun vsota_el(sez: int list) =
    if null sez
    then 0
    else hd sez + vsota_el(tl sez)

(* n-ti element seznama *)
fun n_ti_element(sez: int list, n: int) =
    if n=1
    then hd sez
    else n_ti_element(tl sez, n-1)

(* konkatenacija seznamov - append *)
fun zdruzi_sez(sez1: int list, sez2: int list) =
    if null sez1
    then sez2
    else (hd sez1)::zdruzi_sez(tl sez1, sez2)

(* prepletemo seznama v terke do dolzine krajsega od seznamov *)
fun prepleti_sez(sez1: int list, sez2: int list) =
    if null sez1 orelse null sez2
    then []
    else (hd sez1, hd sez2)::prepleti_sez(tl sez1, tl sez2)

(* vsota parov elementov v terkah vzdolz seznama *)
fun vsota_parov(sez: (int*int) list) =
    if null sez
    then []
    else (#1 (hd sez) + #2 (hd sez))::vsota_parov(tl sez)

(* filtiranje imen predmetov, kjer smo dobili pozitivno oceno *)
fun filter_poz_ocen(sez: (string*int) list) =
    if null sez
    then []
    else if #2 (hd sez) > 5 
    then (#1 (hd sez))::filter_poz_ocen(tl sez)
    else filter_poz_ocen(tl sez)



(***************************************************************)
(*********************** 3. LOKALNO OKOLJE *********************)
(***************************************************************)

(* eksperiment *)
val a = 3
val b = 7
fun sestej3(c: int) =
    let
	val a = 5
    in
	a + (let val b=4 in b+1 end) + (let val c=b+2 in c end)
    end


(* lokalna funkcija *)
(* uporaba zunanje funkcije *)
fun povprecje(sez: int list) =
    Real.fromInt(vsota_el(sez)) / Real.fromInt(stevilo_el(sez))

(* uporaba notranje pomozne funkcije *)
fun povprecje2(sez: int list) =
    let
	fun stevilo_el(sez: int list) =
	    if null sez
	    then 0
	    else 1 + stevilo_el(tl sez)
	fun vsota_el(sez: int list) =
	    if null sez
	    then 0
	    else hd sez + vsota_el(tl sez)
	val vsota = Real.fromInt(vsota_el(sez))
	val n = Real.fromInt(stevilo_el(sez))
    in
	vsota/n
    end


(* primer: odstranitev odvecnih parametrov *)
fun sestej1N (n: int) =
    let fun sestejAB (a: int, b: int) =  (* pomozna funkcija *)
	    if a=b then a else a+ sestejAB(a+1, b)
    in
	sestejAB(1, n)
    end

fun sestej1N_lepse (n: int) =
    let 
	fun sestejAB (a: int) =  (* odstranimo parameter b *)
	    if a=n then a else a + sestejAB(a+1)
    in
	sestejAB(1)
    end



(* ucinkovitost pri rekurziji *)
fun najvecji_el (sez : int list) =
    if null sez
    then 0  (* !!! *)
    else if null (tl sez)
    then hd sez
    else if hd sez > najvecji_el(tl sez)
    then hd sez
    else najvecji_el(tl sez)

fun najvecji_el_popravljen (sez : int list) =
    if null sez
    then 0  (* !!! *)
    else if null (tl sez)
    then hd sez
    else let val max_rep = najvecji_el_popravljen(tl sez)
	 in
	     if hd sez > max_rep
	     then hd sez
	     else max_rep
	 end

	 

(***************************************************************)
(********************** 4. OPCIJE  *****************************)
(***************************************************************)


(* najvecji3: sez -> int option *)
fun najvecji3 (sez : int list) =
    if null sez
    then NONE
    else let val max_rep = najvecji3(tl sez)
	 in
	     if isSome(max_rep) andalso valOf(max_rep) > hd sez
	     then max_rep
	     else SOME (hd sez)
	 end


(* poiscemo prvo lokacijo pojavitve elementa el *)
fun najdi_slab(sez: int list, el: int) =
    if null sez
    then ~1
    else if (hd sez = el)
    then 1
    else let val preostanek = najdi_slab(tl sez, el)
	 in if preostanek = ~1
	    then ~1
	    else 1 + preostanek
	 end


(* (int list * int) -> int option *)
fun najdi(sez: int list, el: int) =
    if null sez
    then NONE
    else if (hd sez = el) (* element je v glavi *)
    then SOME 1
    else let val preostanek = najdi (tl sez, el)
	 in if isSome preostanek
	    then SOME (1+ valOf preostanek)
	    else NONE
	 end


	
(***************************************************************)
(********************** 5. ZAPISI  *****************************)
(***************************************************************)

fun izpis_studenta (zapis: {absolvent:bool, ime:string, ocene:(string * int) list, starost:int}) =
    (#ime zapis) ^ " je star " ^  Int.toString(#starost zapis) ^ " let."


(***************************************************************)
(************** 6. SINONIMI PODATKOVNIH TIPOV  *****************)
(***************************************************************)


(* primer 1: student *)
type student = {absolvent:bool, ime:string, ocene:(string * int) list, starost:int}

fun izpis_studenta2 (zapis: student) =
    (#ime zapis) ^ " je star " ^  Int.toString(#starost zapis) ^ " let."



(* primer 2: artikli v trgovini *)
type artikel = string * int

fun najmanj2mleka (a: artikel) =
    (#1 a = "mleko") andalso (#2 a >=2)   

fun prestejizdelke(sa: artikel list): int =
    if null sa
    then 0
    else #2 (hd sa) + prestejizdelke(tl sa)




(***************************************************************)
(******************* 7. TERKE SO ZAPISI  ***********************)
(***************************************************************)

val test = {1="Zivjo", 2="adijo"}; (* enakovredno terki *)




(***************************************************************)
(*************** 8. DEKLARACIJA LASTNIH TIPOV  *****************)
(***************************************************************)
datatype prevozno_sredstvo = Bus of int
			   | Avto of string * string 
			   | Pes

fun obdelaj_prevoz x =
    case x of
	Bus i => i+10
      | Avto (s1,s2) => String.size s1 + String.size s2
      | Pes => 0

fun obdelaj_prevoz2 x =
    case x of
	Bus i => i+10
      | Avto (s1,s2) => String.size s1 + String.size s2
      | Pes => 0 



(* PRIMER: ARITMETIČNI IZRAZI **********************************)

datatype izraz =  Konstanta of int 
		| Negiraj of izraz
		| Plus of izraz * izraz
		| Minus of izraz * izraz
		| Krat of izraz * izraz
		| Deljeno of izraz * izraz
		| Ostanek of izraz * izraz
				      
val izraz1 = Konstanta 3
val izraz2 = Negiraj (Konstanta 3)
val izraz3 = Plus (Konstanta 3, Ostanek(Konstanta 18, Konstanta 4))
val izraz4 = Deljeno (izraz3, Negiraj izraz2)

fun eval e =
    case e of
        Konstanta i => i
      | Negiraj e  => ~ (eval e)
      | Plus(e1,e2) => (eval e1) + (eval e2)
      | Minus(e1,e2) => (eval e1) - (eval e2)
      | Krat(e1,e2) => (eval e1) * (eval e2)
      | Deljeno(e1,e2) => (eval e1) div (eval e2)
      | Ostanek(e1,e2) => (eval e1) mod (eval e2)

fun stevilo_negacij e =
    case e of
	Konstanta i => 0
      | Negiraj e  => (stevilo_negacij e) + 1
      | Minus(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)
      | Krat(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)
      | Deljeno(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)
      | Ostanek(e1,e2) => (stevilo_negacij e1) + (stevilo_negacij e2)




(***************************************************************)
(************* 9. SEZNAMI IN OPCIJE KOT DATATYPE  **************)
(***************************************************************)
 
(* definicija lastnega seznama *)
datatype mojlist = konec
       | Sez of int * mojlist

(* definicija lastne opcije *)
datatype intopcija = SOME of int
		   | NONE


(* SEZNAM - ujemanje vzorcev *)
fun glava sez =
    case sez of
	[] => 0 (* !!! kasneje: exception *)
     | prvi::ostali => prvi

fun prestej_elemente sez =
    case sez of
	[] => 0
     | glava::rep => 1 + prestej_elemente rep


(* OPCIJE - ujemanje vzorcev *)
fun vecji_od_5 opcija =
    case opcija of
	NONE => false
     | SOME x => (x>5)



(***************************************************************)
(****************  10. POLIMORFIZEM POD. TIPOV  *****************)
(***************************************************************)

datatype ('a, 'b) seznam =
	 Elementa of ('a * ('a, 'b) seznam)
       | Elementb of ('b * ('a, 'b) seznam)
       | konec

(* fn: seznam -> (int * int) *)
fun prestej sez =
    case sez of
	Elementa(x, preostanek) => let val vp = prestej(preostanek)
				   in (1+ (#1 vp), #2 vp)
				   end
      | Elementb(x, preostanek) => let val vp = prestej(preostanek)
				   in (#1 vp, 1+ (#2 vp))
				   end
      | konec => (0,0)



	  
(***************************************************************)
(************* 11. UJEMANJE VZORCEV PRI DEKLARACIJAH ************)
(***************************************************************)
val (a,b,c) = (1,2,3);
val (a,b) = (3,(true,3.14));
val (a,(b,c)) = (3,(true,3.14));
val {prva=a, tretja=c, druga=b} = {prva=true, druga=false, tretja=3};
val glava::rep = [1,2,3,4];
val prvi::drugi::ostali = [1,2,3,4,5];

fun sestej1 (trojcek: int*int*int) =
    let val (a,b,c) = trojcek
    in a+b+c
    end

fun sestej2 (a,b,c) =   (* izvede ujemanje vzorca tukaj *)
    a + b + c



(***************************************************************)
(******** 12. FUNKCIJE SPREJEMAJO SAMO 1 ARGUMENT ***************)
(***************************************************************)

fun povecaj (a,b,c) = (a+1,b+1,c+1)



(***************************************************************)
(*************** 13. REKURZIVNO UJEMANJE VZORCEV  ***************)
(***************************************************************)


(* uporaba anonimne spremenljivke, kjer ne potrebujemo vrednosti *)
fun dolzina (sez:int list) =
    case sez of
       [] => 0
      | (* NAMESTO: glava::rep *) _::rep => 1 + dolzina rep


(***************************************************************)
(* 1. PRIMER *)
(* seštevanje dveh seznamov po elementih; seznama morata biti enako dolga *)

(* SLAB NACIN: *)
exception LengthProblem

fun sestej_seznama (sez1, sez2) =
    case sez1 of
	[] => (case sez2 of
		   [] => []
		 | glava::rep => raise LengthProblem)
      | glava1::rep1 => (case sez2 of
			     [] => raise LengthProblem
			   | glava2::rep2 => (glava1+glava2)::sestej_seznama(rep1,rep2))

(* BOLJSI NACIN z gnezdenjem vzorcev*)
fun sestej_seznama2 seznama =
    case seznama of
	([], []) => []
      | (glava1::rep1, glava2::rep2) => (glava1+glava2)::sestej_seznama(rep1,rep2)
      | _ => raise LengthProblem


(***************************************************************)
(* 2. PRIMER *)
fun check_fibonacci sez =
    case sez of
      (glava::(drugi::(tretji::rep))) => (tretji = (glava+drugi)) andalso check_fibonacci (drugi::(tretji::rep))
      | _ => true


(***************************************************************)
(* 3. PRIMER *)
datatype sodost = S | L | N

fun sodost_sestevanje (a,b) = 
  let 
      fun sodost x = if x=0 then N
		     else if x mod 2 = 0 then S 
		     else L
  in
      case (sodost a, sodost b) of
	  (S,L) => L
	| (S,_) => S
	| (L,L) => S
	| (L,_) => L
	| (N,x) => x
  end




(***************************************************************)
(*************** 14. SKLEPANJE NA PODATKOVNI TIP  ***************)
(***************************************************************)

(* ne deluje, unknown flex record *)
(*
fun sestej1 stevili =
  #1 stevili + #2 stevili
*)

(* moramo opredeliti podatkovni tip *)
fun sestej2 (stevili:int*int) =
    #1 stevili + #2 stevili

(* sklepanje na tip deluje pri uporabi vzorcev *)
fun sestej3 (s1, s2) =
    s1 + s2


(***************************************************************)
(* polimorfizem pri sklepanju na tip *)

(* ni polimorfna *)
fun vsota_el sez =
    case sez of
	[] => 0
      | glava::rep => glava + vsota_el rep

(* je polimorfna *)
fun zdruzi (sez1, sez2) =
    case sez1 of
	[] => sez2
      | glava::rep => glava::zdruzi(rep, sez2)

(* je polimorfna *)
fun sestej_zapis {prvi=a, drugi=b, tretji=c, cetrti=d, peti=e} =
    a+d

(* primerjalni podatkovni tip *)
fun f1 (a,b,c,d) =
  if a=b
  then c
  else d
(* rezultat: val f1 = fn : ''a * ''a * 'b * 'b -> 'b *)


(***************************************************************)
(************************* 15. IZJEME  **************************)
(***************************************************************)


(* prej - brez uporabe izjem *)
(*
fun glava sez =
    case sez of
	[] => 0 (* !!! kasneje: exception *)
     | prvi::ostali => prvi
*)

exception PrazenSeznam

fun glava sez =
    case sez of
	[] => raise PrazenSeznam
     | prvi::ostali => prvi


(***************************************************************)
(* primer izjeme pri deljenju z 0 *)
exception DeljenjeZNic

fun deli1 (a1, a2) =
    if a2 = 0 
    then raise DeljenjeZNic
    else a1 div a2

fun tabeliraj1 zacetna =
    deli1(zacetna,zacetna-5)::tabeliraj1(zacetna-1)
    handle DeljenjeZNic => [999]


(***************************************************************)
(* še bolj splošno: prenos izjeme v parametru *)
(* fn : int * int * exn -> int *)
fun deli2 (a1, a2, napaka) =
    if a2 = 0 
    then raise (* SPREMEMBA *) napaka
    else a1 div a2

fun tabeliraj2 (zacetna, moja_napaka) =
    deli2(zacetna, zacetna-5, moja_napaka)::tabeliraj2(zacetna-1, moja_napaka)
    handle moja_napaka => [999]


(***************************************************************)
(* izjema s parametrom *)
exception MatematicnaTezava of int*string

fun deli3 (a1, a2) =
    if a2 = 0 
    then raise MatematicnaTezava(a1, "deljenje z 0")
    else a1 div a2

fun tabeliraj3 zacetna =
    Int.toString(deli3(zacetna,zacetna-5)) ^ "  " ^ tabeliraj3(zacetna-1) 
    handle MatematicnaTezava(a1, a2) => a2 ^ " stevila " ^ Int.toString(a1)