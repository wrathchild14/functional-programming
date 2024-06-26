fun reduce f z [] = z
  | reduce f z (x::xs) = reduce f (f z x) xs

fun squares [] = []
  | squares xs = List.map (fn x => x * x) xs

fun onlyEven [] = []
  | onlyEven xs = List.filter (fn x => x mod 2 = 0) xs

fun bestString f [] = ""
  | bestString f (x::xs) = List.foldl (fn (x, y) => if f(x, y) then x else y) x xs

fun largestString xs = bestString (fn (x, y) => String.compare(x, y) = GREATER) xs

fun longestString xs = bestString (fn (x, y) => size x > size y) xs

fun quicksort _ [] = []
  | quicksort f xs =
    let
      val (smaller, greater) = List.partition (fn y => f(hd xs, y) = LESS) (tl xs)
    in
      quicksort f smaller @ [hd xs] @ quicksort f greater
    end

fun dot xs ys = List.foldl (fn(x, y) => x + y) 0 (ListPair.map(fn (x, y) => x * y) (xs, ys))

fun transpose matrix = 
    case matrix of
      [] => []
    | [] :: _ => []
    | _ =>
      List.map hd matrix :: transpose (List.map tl matrix)

fun multiply m1 m2 = 
      List.map (fn row => List.map (fn col => dot row col) (transpose m2)) m1

fun group [] = []
  | group (x::xs) =
    let
      fun count(_, _) [] = []
        | count (current, counter) (y::ys) = 
          if x = y 
          then count (current, counter + 1) ys
          else (current, counter) :: count (y, 1) ys
    in
      count (x, 1) xs
    end
