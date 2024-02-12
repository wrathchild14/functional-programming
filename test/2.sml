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

fun vsakdrug sez = 
  let
    fun helper ([], acc) = List.rev acc
      | helper ([x], acc) = helper([], x::acc)
      | helper (x1::x2::xs, acc) = helper(xs, x2::(x1 + 10)::acc)
  in
    helper(sez, [])
  end;

vsakdrug [4,5,6,7,8,9,8,7,6];


fun vsakdrug sez = 
  let
    fun helper ([], _, acc) = acc
      | helper (x::xs, true, acc) = helper(xs, false, acc @ [x + 10])
      | helper (x::xs, false, acc) = helper(xs, true, acc @ [x])
  in
    helper(sez, true, [])
  end;

vsakdrug [4,5,6,7,8,9,8,7,6];

fun sestej1 (trojcek: int*int*int) = 
    let 
        val (a,b,c) = trojcek
    in a + b + c
    end

fun sestej2 (a, b, c) = a + b + c

fun testna a {a=b, b=c} = 
    List.map (fn x => a + (valOf b) - c(a)) 
