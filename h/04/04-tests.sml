(* val test_reduce = reduce (fn x y => x + y) 0 [1,2,3]; *)
val test_squares = squares [2, 6, 3]
val test_even = onlyEven [2, 5, 3, 4]
val test_longeststring = longestString ["apple", "banana", "kiwi", "orange", "asdsadasdasd"]

fun intCompare (x, y) =
  if x < y then LESS
  else if x > y then GREATER
  else EQUAL
val sortedList = quicksort intCompare [4, 2, 7, 1, 9, 5]

val matrix1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
val matrix2 = [[9, 8, 7], [6, 5, 4], [3, 2, 1]]
val transpose_result = transpose matrix1
val result1 = multiply matrix1 matrix2

val test_group = group [1, 1, 2, 3, 3, 3, 4, 4, 4, 4]
