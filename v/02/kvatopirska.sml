datatype barva = Kriz | Pik | Srce | Karo
datatype stopnja = As | Kralj | Kraljica | Fant | Stevilka of int

type karta = stopnja * barva

fun barvaKarte ((_, b) : karta) = b;

fun veljavnaKarta ((Stevilka i, _) : karta) = i >= 2 andalso i <= 10 | veljavnaKarta _ = true;

fun vrednostKarte ((s, _) : karta) = case s of Stevilka i => i | As => 11 | _ => 10;

(* fun vsotaKart (nil : karta list) = 0 | vsotaKart (c :: cs) = vrednostKarte c + vsotaKart cs; *)
fun vsotaKart [] = 0
  | vsotaKart(k::karte)= vsotaKart karte + vrednostKarte k

(* fun isteBarve (((_, b1) :: (r as (_, b2) :: ks)) : karta list) = b1 = b2 andalso isteBarve r
  |   isteBarve _ = true; *)
fun isteBarve (k1 :: (karte as k2 :: _)) =
    barvaKarte k1 = barvaKarte k2 andalso isteBarve karte
  | isteBarve( [] | [_]) = true

val karta = (Kralj, Srce)
val kartaList = [(Kralj, Srce), (Kralj, Srce), (Kralj, Srce), (Kralj, Srce), (Kralj, Srce)]
