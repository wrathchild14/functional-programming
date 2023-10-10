val a = 3;
val b = 7;
val d = 3;

fun sestej2 (c:int) =
        let
            val a = 5
            val b = a + c + 1
        in
            a + b + c + d
        end

fun sestej3 (c: int) =
        let
            val a = 5
        in
            a + (let val b = 4 in b + 1 end) + (let val c = b + 2 in c end)
        end

val test = sestej3 1;