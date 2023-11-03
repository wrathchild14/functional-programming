datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip([], []) = []
  | zip([], _::_) = []
  | zip(_::_, []) = []
  | zip(x::xs, y::ys) = (x, y) :: zip(xs, ys);

fun unzip [] = ([], [])
  | unzip((x, y)::rest) =
    let
      val (xs, ys) = unzip(rest)
    in
      (x::xs, y::ys)
    end;

fun subtract(a, b) =
    case (a, b) of
      (One, _) => raise NotNaturalNumber
    | (Succ x, One) => x
    | (Succ x, Succ y) => subtract(x, y)

fun any(_, []) = false
  | any(f, x::xs) = if f(x) then true else any(f, xs);

fun map(_, []) = []
  | map(f, x::xs) = f(x)::map(f, xs)

fun filter(_, []) = []
  | filter(f, x::xs) =
    if f(x) then x::filter(f, xs)
    else filter(f, xs);

fun fold(_, z, []) = z
  | fold(f, z, x::xs) = fold(f, f(z, x), xs)

fun rotate(tree, dir) =
    case (tree, dir) of
      ((br (br (ls, xs, rs), x, r)), R) => br (ls, xs, br (rs, x, r))
    | ((br (l, x, br (ls, xs, rs)), L)) => br (br (l, x, ls), xs, rs)
    | (tree, _) => tree;
