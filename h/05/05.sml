signature RATIONAL =
sig
  (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
  eqtype rational

  (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
  exception BadRational

  (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
  val makeRational: int * int -> rational

  (* Vrne nasprotno vrednost podanega števila. *)
  val neg: rational -> rational

  (* Vrne obratno vrednost podanega števila. *)
  val inv: rational -> rational

  (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
  val add: rational * rational -> rational
  val mul: rational * rational -> rational

  (* Vrne niz, ki ustreza podanemu številu.
     Če je število celo, naj vrne niz oblike "x" oz. "~x".
     Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
  val toString: rational -> string
end

structure Rational :> RATIONAL =
struct

  exception BadRational
  datatype rational = Rationals of int * int

  fun gcd (a , b)=
      if b = 0 then a
      else gcd(b, a mod b)

  fun makeRational (x , y) =
      if y = 0 then raise BadRational
      else Rationals(x div gcd (x, y), y div gcd (x, y))

  fun neg (Rationals (x, y)) = Rationals(~x, y)
    
  fun inv (Rationals (x, y)) =
      if x = 0 then raise BadRational
      else if y = 0 then raise BadRational
        else if x < 0 then
            if y < 0 then Rationals (~y, ~x)
            else Rationals (~y, ~x)
          else if y < 0 then Rationals (~y, ~x)
            else Rationals (y, x)

  fun add (Rationals (x1, y1), Rationals (x2, y2)) =
      let
        val common_denominator = y1 * y2
        val xx = x1 * (common_denominator div y1) + x2 * (common_denominator div y2)
      in
        makeRational (xx, common_denominator)
      end

  fun mul (Rationals (x1, y1), Rationals (x2, y2)) =
      makeRational (x1 * x2, y1 * y2)

  fun toString (Rationals (x, y)) =
      if y = 1 then Int.toString x
      else if x = 0 then "0"
        else if x < 0 then
            if y < 0 then "~" ^ Int.toString (~x) ^ "/" ^ Int.toString (~y)
            else "~" ^ Int.toString (~x) ^ "/" ^ Int.toString y
          else if y < 0 then
              Int.toString x ^ "/~" ^ Int.toString (~y)
            else
              Int.toString x ^ "/" ^ Int.toString y
end


signature EQ =
sig
  type t
  val eq : t -> t -> bool
end

signature SET =
sig
  (* podatkovni tip za elemente množice *)
  type item

  (* podatkovni tip množico *)
  type set

  (* prazna množica *)
  val empty : set

  (* vrne množico s samo podanim elementom *)
  val singleton : item -> set

  (* unija množic *)
  (* val union : set -> set -> set *)

  (* razlika množic (prva - druga) *)
  (* val difference : set -> set -> set *)

  (* a je prva množica podmnožica druge *)
  (* val subset : set -> set -> bool *)
end

(* funsig SETFN (Eq : EQ) = SET *)

functor SetFn(Eq: EQ) :> SET =
struct
  type item = Eq.t

  type set = item list

  val empty = []

  fun singleton x = [x]

end
