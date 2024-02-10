datatype 'a thunk = Thunk of (unit -> 'a) | Value of 'a
fun mydelay f = ref (false, f)

fun myforce refcell =
    case !refcell of
        (false, Thunk f) => 
                let 
                    val x = f ()
                in 
                    refcell := (true, Value x); x
                end
      | (true, Value x) => x

val x = mydelay (Thunk (fn () => 1 + 2));
val y = myforce x;
val z = myforce x;