val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;


fun readFile filename =
    let val is = TextIO.openIn filename
    in 
      String.map (fn c => if Char.isGraph c orelse c = #" " orelse c = #"\n" then c else #" ")
      (TextIO.inputAll is)
      before TextIO.closeIn is
    end

exception NotImplemented;

fun split n xs =
    let
      val first  = List.take(xs, n) handle Subscript => []
      val rest = List.drop(xs, n) handle Subscript => []
    in
      if length first > 0 then
        first :: split n rest
      else
        []
    end

fun xGCD(a, b) =
    let
      val (mutable_s, mutable_old_s) = (ref 0, ref 1)
      val (mutable_r, mutable_old_r) = (ref b, ref a)
      val quotient = ref 0
      val bezout_t = ref 0
    in
      (
        while !mutable_r <> 0 do
          (
            quotient := !mutable_old_r div !mutable_r;

            let
              val temp_r = !mutable_r
              val temp_s = !mutable_s
            in
              mutable_r := !mutable_old_r - (!quotient * !mutable_r);
              mutable_old_r := temp_r;

              mutable_s := !mutable_old_s - (!quotient * !mutable_s);
              mutable_old_s := temp_s
            end;

            if b <> 0 then
              bezout_t := (!mutable_old_r - !mutable_old_s * a) div b
            else
              bezout_t := 0
          )
      );

      (!mutable_old_r, !mutable_old_s, !bezout_t)
    end

signature RING =
sig
  eqtype t
  val zero : t
  val one : t
  val neg : t -> t
  val xGCD : t * t -> t * t * t
  val inv : t -> t option
  val + : t * t -> t
  val * : t * t -> t
end;

functor Ring (val n : int) :> RING where type t = int =
struct
  type t = int
  val zero = 0
  val one = 1
  fun neg x = ~x mod n
  val xGCD = xGCD
  
  fun inv x =
      case xGCD (x mod n, n) of
        (1, s, _) => SOME (s mod n)
      | _ => NONE

  fun op + a =  Int.+ a mod n
  fun op * p =  Int.* p mod n
end;

signature MAT =
sig
  eqtype t
  structure Vec :
  sig
    val dot : t list -> t list -> t
    val add : t list -> t list -> t list
    val sub : t list -> t list -> t list
    val scale : t -> t list -> t list
  end
  val tr : t list list -> t list list
  val mul : t list list -> t list list -> t list list
  val id : int -> t list list
  val join : t list list -> t list list -> t list list
  val inv : t list list -> t list list option
end;

functor Mat (R : RING) :> MAT where type t = R.t =
struct
  type t = R.t
  structure Vec =
  struct
    fun dot a b = foldl R.+ R.zero (ListPair.map R.* (a,b))
    fun add a b = ListPair.map R.+ (a, b)
    fun sub a b = ListPair.map (fn (x, y) => R.+ (x, R.neg y)) (a, b)
    fun scale a b = map (fn x => R.* (a, x)) b
  end

  fun tr m = 
      case m of 
        [] => []
      | [] :: _ => []
      | _ => map hd m :: tr (map tl m)
  fun mul m1 m2 = map (fn row => map (Vec.dot row) (tr m2)) m1
  fun id n =
      List.tabulate(n, fn i =>
          List.tabulate(n, fn j =>
              if i = j then R.one else R.zero
            ))
  (* let
      fun generateRow i j = if i = j then R.one else R.zero
      fun generateIdRow i = List.tabulate(n, (generateRow i))
    in
      List.tabulate(n, generateIdRow)
    end *)
  fun join m1 m2 = 
      if length m1 = length m2 then 
        ListPair.map(fn (row1, row2) => row1 @ row2) (m1, m2) 
      else 
        m1
  fun inv matrix = 
      let
        fun reduce v m = map (fn x :: u => Vec.sub u (Vec.scale x v) | _ => raise Empty) m

        fun pivot ((v as x :: _ ):: m) =
            (case R.inv x of 
                SOME x' => SOME (Vec.scale x' v :: m)
              | NONE =>
                case m of
                  ((u as y :: _) :: m') =>
                    let val (g, s, t) = R.xGCD (x, y) in
                      case pivot (Vec.add (Vec.scale s v) (Vec.scale t u) :: m') of
                        SOME (w :: m'') => SOME (w :: u :: v :: m'')
                      | _ => NONE
                    end
                | _ => NONE)
          | pivot _ = NONE

        fun gauss (above, []) = SOME above
          | gauss (above, below) = 
            case pivot below of 
              SOME ((_ :: v) :: m) => gauss (reduce v above @ [v], 
                  List.filter (List.exists (fn x => x <> R.zero)) (reduce v m))
            | _ => NONE
      in
        gauss([], (join matrix (id (length matrix))))
      end
end;

signature CIPHER =
sig
  type t
  val encrypt : t list list -> t list -> t list
  val decrypt : t list list -> t list -> t list option
  val knownPlaintextAttack : int -> t list -> t list -> t list list option
end;

functor HillCipherAnalyzer (M : MAT) :> CIPHER
  where type t = M.t
  =
struct
  type t = M.t
  
  fun encrypt key plaintext =
      let
        val size = length key
        val blocks = split size plaintext
        val encrypted = List.map (fn block => hd (M.mul [block] key)) blocks
      in
        List.concat encrypted
      end

  fun decrypt key ciphertext = 
      let
        (* val blocks = split (length key) ciphertext *)
        val invm = case M.inv key of SOME inv => inv | NONE => [[]]
      in
        case invm of
          [[]] => NONE
          (* | _ => SOME (List.concat(List.foldr (fn (block, acc) => (hd (M.mul [block] invm))::acc) [] blocks)) *)
        | _ => SOME(encrypt invm ciphertext) (* the same *)
      end

  fun knownPlaintextAttack keyLength plaintext ciphertext = 
      let
        val x = split keyLength plaintext
        val y = split keyLength ciphertext

        val xx = List.take(x, keyLength) handle Subscript => [[]]
        val yy = List.take(y, keyLength) handle Subscript => [[]]
        val xinv = (M.inv xx)

        fun attack_alt rows = 
            let 
              val x_ = List.take(x, rows) handle Subscript => [[]]
              val y_ = List.take(y, rows) handle Subscript => [[]]

              val mx_ = (M.mul (M.tr x_) x_)
              val my_ = (M.mul (M.tr y_) y_)

              val xinv_ = (M.inv mx_)
            in 
              case xinv_ of
                (* SOME minv => if M.mul x (M.mul minv my_) = y then SOME (M.mul minv my_) else attack_alt (rows + 1) *)
                (* SOME minv => SOME (M.mul minv my_) *)
                SOME minv => SOME (M.mul minv (M.mul (M.tr x_) y_))
              | NONE => 
                if rows < length x andalso rows < length y then 
                  attack_alt (rows+1)
                else NONE
            end
      in
        if length plaintext <> length ciphertext orelse length plaintext mod keyLength <> 0 then
          NONE
        else
          if length x > 1 andalso length y > 1 then
            (* case xinv of *)
            (* SOME minv => if M.mul x (M.mul minv y) = y then SOME(M.mul minv y) else attack_alt 1
            | NONE => attack_alt 1  *)
            (* | NONE => case attack_alt 1 of
                          SOME minv' => if M.mul x (M.mul minv' y) = y then SOME(M.mul minv' y) else NONE
                        | NONE => NONE                       *)
            case xinv of
              SOME minv => SOME (M.mul minv yy)
            | NONE => attack_alt 1
          else NONE
      end
end;


structure Trie :> 
sig
  eqtype ''a dict
  val empty : ''a dict
  val insert : ''a list -> ''a dict -> ''a dict
  val lookup : ''a list -> ''a dict -> bool
end
  =
struct
  datatype ''a tree = N of ''a * bool * ''a tree list
  type ''a dict = ''a tree list

  val empty = [] : ''a dict

  fun insert [] dict = dict
    | insert (x::xs) (N(y, flag, children) :: rest) =
      if x = y then
        if null xs then
          N(y, true, children) :: rest
        else
          N(y, flag, insert xs children) :: rest
      else
        N(y, flag, children) :: insert (x::xs) rest
    | insert (x::xs) dict = [N(x, null xs, insert xs dict)]

  fun lookup [] [] = true
    | lookup [] _ = false
    | lookup _ [] = false
    | lookup [x] (N(y, flag, _) :: _) = x = y andalso flag
    | lookup (x::xs) (N(y, _, children) :: rest) =
      if x = y then
        lookup xs children
      else
        lookup (x::xs) rest
end;

signature HILLCIPHER =
sig
  structure Ring : RING where type t = int
  structure Matrix : MAT where type t = Ring.t
  structure Cipher : CIPHER where type t = Matrix.t
  val alphabetSize : int
  val alphabet : char list
  val encode : string -> Cipher.t list
  val decode : Cipher.t list -> string
  val encrypt : Cipher.t list list -> string -> string
  val decrypt : Cipher.t list list -> string -> string option
  val knownPlaintextAttack :
    int -> string -> string -> Cipher.t list list option
  val ciphertextOnlyAttack : int -> string -> Cipher.t list list option
end

functor HillCipher (val alphabet : string) :> HILLCIPHER =
struct

  (*printable characters*)
  val alphabetSize = String.size alphabet
  val alphabet = String.explode alphabet

  structure Ring = Ring (val n = alphabetSize)
  structure Matrix = Mat (Ring)
  structure Cipher = HillCipherAnalyzer (Matrix)

  fun encode txt =
      let
        fun charToIndex _ _ [] = raise Option
          | charToIndex c counter (alphabet_c::rest) = 
            if alphabet_c = c then counter
            else charToIndex c (counter + 1) rest
      in
        List.map (fn c => charToIndex c 0 alphabet) (String.explode txt)
      end

  fun decode code =
      let
        fun indexToChar i =
            if i < length alphabet then List.nth (alphabet, i)
            else raise Option
      in
        String.implode (List.map indexToChar code)
      end

  local
    fun parseWords filename =
        let val is = TextIO.openIn filename
          fun read_lines is =
              case TextIO.inputLine is of
                SOME line =>
                  if String.size line > 1
                  then String.tokens (not o Char.isAlpha) line @ read_lines is
                  else read_lines is
              | NONE => []
        in List.map (String.map Char.toLower) (read_lines is) before TextIO.closeIn is end

    val dictionary = List.foldl (fn (w, d) => Trie.insert w d) Trie.empty (List.map String.explode (parseWords "hamlet.txt")) handle NotImplemented => Trie.empty
  in
    fun encrypt key plaintext = decode (Cipher.encrypt key (encode plaintext))
    fun decrypt key ciphertext = 
        case Cipher.decrypt key (encode ciphertext) of
          SOME bla => SOME (decode bla)
        | NONE => NONE
    fun knownPlaintextAttack keyLenght plaintext ciphertext = Cipher.knownPlaintextAttack keyLenght (encode plaintext) (encode ciphertext)
    fun ciphertextOnlyAttack keyLenght ciphertext = raise NotImplemented
  end
end;
