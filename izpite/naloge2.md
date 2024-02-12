NALOGA 1 (10 točk / 70)
Na predavanjih smo se pogovarjali o tem, kako SML evalvira funkcijski klic oblike
e(arg1, arg2, ..., argn)
To evalvacijo lahko strnemo v 3 korake. Spodaj so podane različne možnosti za te tri korake. Najdi
pravilne možnosti in ustrezno označi njihovo zaporedje kot KORAK 1, KORAK 2 in KORAK 3. Če
ponujena možnost ne sodeluje v postopku, jo prečrtaj!
o evalviraj argumente arg1, arg2, ..., argn v vrednosti formalnih argumentov
o evalviraj telo funkcije v okolju, ki vsebuje samo formalne argumente
o evalviraj telo funkcije, pri čemer za vrednosti formalnih argumentov uporabi vrednosti x1, x2, ...,
xn
o evalviraj izraz e v ime, formalne argumente in telo funkcije
o evalviraj formalne argumente v vrednosti arg1, arg2, ..., argn
o evalviraj telo funkcije v okolju, kjer je funkcija klicana
o evalviraj argumente arg1, arg2, ..., argn v vrednosti x1, x2, ..., xn
o evalviraj izraz e v ime funkcije
o evalviraj izraz e v ime in telo funkcije


KORAK 1: evalviraj izraz e v ime, formalne argumente in telo funkcije
KORAK 2: evalviraj argumente arg1, arg2, ..., argn v vrednosti formalnih argumentov
KORAK 3: evalviraj telo funkcije, pri čemer za vrednosti formalnih argumentov uporabi vrednosti x1, x2, ..., xn

NALOGA 2 (11 točk / 70)
V SML je podan naslednji program:
datatype 'a Tip = A of (int * 'a)
                | B of ('a -> int)
fun mojamoja a b =
    case a of
        B x => B x
       | _ => A(3, 3)
Določi podatkovni tip funkcije mojamoja

'a Tip -> 'b -> 'a Tip
'a Tip can be either int * int for A or 'a -> int for B

NALOGA 3 (8 točk / 70)
Kateri od naslednjih programov v SML uporabljajo repno rekurzijo?
Izberi enega ali več odgovorov:
```
fun prestejpoz sez =
 let
fun pomozna (sez, acc) =
 case sez of
 [] => acc
| g::rep => if g>=0
then pomozna(rep, acc+1)
else pomozna(rep, acc)
 in
pomozna(sez,0)
 end
fun zmnozi_nkrat (x,n) =
 if n=0
 then x
 else x * zmnozi_nkrat(x, n-1)
fun zadnji sez =
 case sez of
nil => NONE
 | [x] => SOME x
 | _::r => zadnji r
fun filter (f, sez) =
 case sez of
[] => []
 | glava::rep => if (f glava)
 then glava::filter(f, rep)
 else filter(f, rep)
fun fib (n:int) =
 if n=1 then 1
 else if n=2 then 1
 else fib(n-1) + fib(n-2)
 ```

prestejpoz: This function uses tail recursion. The recursive call to pomozna is the last operation in each branch of the if statement.

zmnozi_nkrat: This function does not use tail recursion. The recursive call to zmnozi_nkrat is not the last operation because it's part of a multiplication operation.

zadnji: This function uses tail recursion. The recursive call to zadnji is the last operation in the _::r case.

filter: This function uses tail recursion. The recursive call to filter is the last operation in each branch of the if statement.

fib: This function does not use tail recursion. There are two recursive calls to fib, and neither of them is the last operation in the function.


NALOGA 4 (9 točk / 70)
Razvrsti naslednje programske jezike glede na način tipizacije.
Za vsak našteti programski jezik obkroži ustrezno kategorijo v desnih treh stolpcih!
Java statično/dinamično tipiziran močno/šibko tipiziran implicitno/eksplicitno tipiziran
SML statično/dinamično tipiziran močno/šibko tipiziran implicitno/eksplicitno tipiziran
Racket statično/dinamično tipiziran močno/šibko tipiziran implicitno/eksplicitno tipiziran
C++ statično/dinamično tipiziran močno/šibko tipiziran implicitno/eksplicitno tipiziran
Python statično/dinamično tipiziran močno/šibko tipiziran implicitno/eksplicitno tipiziran
JavaScript statično/dinamično tipiziran močno/šibko tipiziran implicitno/eksplicitno tipiziran

SML
Statično tipiziran (Statically typed): Types are checked at compile-time.
Močno tipiziran (Strongly typed): Types cannot be implicitly converted or interchanged.
Implicitno tipiziran (Implicitly typed): Types of variables do not need to be explicitly declared, they are inferred by the compiler.
Racket
Dinamično tipiziran (Dynamically typed): Types are checked at runtime.
Močno tipiziran (Strongly typed): Types cannot be implicitly converted or interchanged.
Implicitno tipiziran (Implicitly typed): Types of variables do not need to be explicitly declared.



NALOGA 5 (13 točk / 70)
V spodnji definiciji je podan podatkovni tip vozlisce, ki predstavlja vozlišče drevesa s tremi
komponentami: levo poddrevo, vrednost v vozlišču in desno poddrevo. Poljubno poddrevo se lahko
konča z vrednostjo null, ki označuje list drevesa.
Poleg definicije podatkovnega tipa je podana tudi posplošena funkcija za delo s seznami:
datatype vozlisce = v of (vozlisce * int * vozlisce)
 | null
fun opdrevo operacija trenutni nevtralni drevo =
 case drevo of
 null => nevtralni
 | v(levo,x,desno) =>
 operacija(opdrevo operacija trenutni nevtralni levo,
operacija(opdrevo operacija trenutni nevtralni desno,
 trenutni x))
Zapiši naslednje tri delne aplikacije zgornje posplošene funkcije:
1. sestejd: sešteje vse elemente v drevesu,
2. zmnozid: zmnoži vse elemente v drevesu,
3. prestejd: prešteje vse elemente v drevesu,
Primeri izvajanja teh delnih aplikacij so:
- sestejd (v(v(null,1,null),3,v(null,2,v(null,1,null))));
val it = 7 : int
- zmnozid (v(v(null,1,null),3,v(null,2,v(null,1,null))));
val it = 6 : int
- prestejd (v(v(null,1,null),3,v(null,2,v(null,1,null))));
val it = 4 : int

fun sestejd drevo = opdrevo (op +) (fn x => x) 0 drevo;
fun zmnozid drevo = opdrevo (op *) (fn x => x) 1 drevo;
fun prestejd drevo = opdrevo (fn _ => fn _ => fn y => 1) (fn x => x) 0 drevo;


NALOGA 6 (10 točk / 70)
Na predavanjih smo za računanje Fibonaccijevega zaporedja napisali naslednjo funkcijo, ki uporablja
tehniko memoizacije:
(define fibo
 (letrec ([solutions null]
 [fibo1 (lambda (x)
 (let ([ans (assoc x solutions)])
 (if ans
 (cdr ans)
 (let ([nova (cond [(= x 1) 1]
 [(= x 2) 1]
 [#t (+ (fibo1 (- x 1))
 (fibo1 (- x 2)))])])
 (begin
 (set! solutions (cons (cons x nova) solutions))
 nova)))))])
 fibo1))
Kaj je vsebina seznama solutions tik pred koncem programa, če izvedemo klic (fibo 5)?
Izberi en odgovor:
 ((4 . 3) (3 . 2) (1 . 1) (2 . 1))
 ((1 2 3 4 5) . (1 1 2 3 5))
 ((1 . 1) (2 . 1) (3 . 2) (4 . 3) (5 . 5))
 (1 1 2 3 . 5)
 (1 1 2 3 5)
 ((5 4 3 2 1) . (5 3 2 1 1))
 ((5 . 5) (4 . 3) (3 . 2) (2 . 1) (1 . 1))
 (5 3 2 1 1)
 ((5 4 3 2 . 1) . (5 3 2 1 . 1))
 (5 3 2 1 . 1)
 ((5 . 5) (4 . 3) (3 . 2) (1 . 1) (2 . 1))
 ((1 2 3 4 . 5) . (1 1 2 3 . 5))

((5 . 5) (4 . 3) (3 . 2) (2 . 1) (1 . 1))


1. NALOGA (20 točk)
Odgovori na naslednja vprašanja (na kratko, z 1-2 povedima):
a) Naštej primere treh napak, ki jih lahko zazna statična analiza programa.

Syntax errors: These are mistakes in the code's syntax, such as missing semicolons or mismatched parentheses.
Type errors: These occur when an operation is performed on an incompatible type, such as trying to add a string to an integer.
Unused variables: These are variables that are declared but never used in the code.
b) Pri kateri paradigmi programiranja (objektno-usmerjeni ali funkcijski) je program lažje
razširjati z novimi podatkovnimi tipi, zakaj?

Functional programming makes it easier to extend the program with new data types. This is because in functional programming, data and behavior are separate. You can add new data types without changing existing functions, and you can add new functions without changing existing data types.
c) Ali drži trditev: »Vsak poln sistem tipov sprejme vse programe, ki jih sprejme tudi trden
sistem?« Pojasni.

The statement is not necessarily true. A complete type system accepts all programs that are well-typed according to its rules. A sound type system rejects all programs that could lead to type errors. Therefore, a complete type system may reject some programs that a sound system would accept, if those programs could potentially lead to type errors.
d) Kaj je to naivna makro razširitev?
 Naive macro expansion is a method of code generation where macros are replaced with their definitions each time they are used. This can lead to code bloat if a macro is used many times, and it can also cause problems if the macro has side effects or if it relies on variables in its surrounding scope.

 But Racket has a good way of cleaning up this with the usage of lexical scope.
 Example: 
(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(let ([tmp 5]
      [other 6])
  (swap tmp other)
  (displayln (list tmp other)))
Result is 6 5

But this is what the macro evaluates to which is 5 6
(let ([tmp 5]
      [other 6])
  (let ([tmp tmp])
    (set! tmp other)
    (set! other tmp))
  (displayln (list tmp other)))

2. NALOGA (15 točk)
V programskem jeziku SML zapiši funkcijo sestej, ki ustreza naslednjim pogojem:
 funkcija naj sprejme dva argumenta (a in b), od katerih je prvi celo število, drugi pa seznam,
 funkcija naj uporablja currying,
 če je prvi argument negativen, naj vrne NONE; sicer naj vrne SOME x, kjer je x vsota seznama
b plus vrednost v spremenljivki a,
 funkcija naj uporablja ujemanje vzorcev.
Zapiši tudi definicijo delne aplikacije funkcije sestej, imenovano sestej3, ki kot seznam b vedno
uporablja seznam [3, 3, 3].

fun sestej a = 
    if a < 0 then fn _ => NONE
    else let
        fun sum [] = a
          | sum (x::xs) = x + sum xs
    in
        fn b => SOME (sum b)
    end;

val sestej3 = sestej 3 [3, 3, 3];


3. NALOGA (15 točk)
V programskem jeziku SML je podan naslednji podatkovni tip:
datatype 'a Tip1 = A of 'a
| B of int
Zapiši funkcijo (poljubno), ki je podatkovnega tipa: 'a * 'b Tip1 -> 'a

fun extractValue (x, A y) = x


2. Podan je naslednji izsek programske kode:
val x = 3
val w = fn _ => x
fun q x =
 let val x = 4
 in w
 end
val rez1 = q 30 15
fun w x = x + 1
val rez2 = q 31 14
Odgovori na vprašanja:
1.) Katerega od sistemov za doseg vrednosti uporablja Standardni ML?
2.) Kakšna je vrednost spremenljivk rez1 in rez2, če predpostavimo, da jezik uporablja dinamični doseg?
rez1=3, rez2=15 (14 + 1 = 15)
3.) Kakšna je vrednost spremenljivk rez1 in rez2, če predpostavimo, da jezik uporablja leksikalni doseg?
rez1=3, rez2=3


3. Naslednje zapise delov programske kode zapiši lepše (skrajšaj kodo ali uporabi sintantične olepšave). Uporabljaj ista
imena spremenljivk, kot so podana v primerih!
1.) if a then true else false
a
2.) fun x y = fun z y
fun x y z = y
3.) {1="racman", 3="jaka", 2=true};
("racman", "jaka", true)
4.) fun f2 a = case a of (a1,a2,a3) => (a1,a2,a3)
 Lepše: fun f2 a = case a of (a1,a2,a3) => ________________
 a
 fun f2 a = a


4. Omejitev vrednosti. Podane so naslednje tri funkcije:
fun f1 x y z = (x andalso y, z)
fun f2 x y z = (x andalso z, y)
fun f3 x y z = (y andalso z, x)
Kateri izmed naslednjih prirejanj v SML ne uspejo zaradi omejitve vrednosti (value restriction)?
Izberi enega ali več odgovorov:
 vl x = f1 true;
 val x = f1 true true;
 val x = f1 true true true;
 val x = f2 true;
 val x = f2 true true;
 val x = f2 true true true;
 val x = f3 true;
 val x = f3 true true;
 val x = f3 true true true; 

val x = f1 true;
val x = f2 true;
val x = f3 true;

7. Funkcijska ovojnica
Kaj od naslednje je vsebovano v funkcijski ovojnici (angl. function closure)?
Izberi enega ali več odgovorov:
a. programska koda funkcije Y 
b. okolje, v katerem je bila funkcija definirana Y
c. povezava do funkcijskega sklada
d. vrednosti argumentov funkcije
e. podatkovni tip funkcije
f. notranje pomožne funkcije Y
g. okolje, iz katerega je funkcija klicana
h. množica vzajemno rekurzivnih funkcij, ki jih funkcija potrebuje Y


3. Katere izjave držijo pri uporabi mehanizma zakasnitve in sprožitve v Racket? Izberi enega ali več odgovorov:
a) klic funkcije za zakasnitev (delay) ne evalvira nobenega argumenta Y
b) klic funkcije za zakasnitev (delay) takoj evalvira svoje argument(e)
c) klic funkcije za zakasnitev (delay) vrne strukturo v obliki obljube (promise) Y
d) klic funkcije za zakasnitev (delay) vrne zakasnitveno funkcijo (thunk) Y
e) funkcijo za sprožitev (force) običajno kličemo, preden se začne izvajati telo funkcije, ki uporablja zakasnjeni izraz
f) klic funkcije za zakasnitev (delay) vrne strukturo v obliki pogodbe (contract)
g) funkcija za sprožitev (force) prejme natanko 3 argumente
h) funkcija za sprožitev (force) prejme natanko 1 argument Y
i) funkciji za zakasnitev (delay) podamo poljuben izraz, katerega evalvacijo želimo zakasniti Y
j) funkcijo za sprožitev (force) običajno kličemo v telesu funkcije Y
k) funkcija za sprožitev (force) prejme natanko 2 argumenta

2. V jeziku Racket je podana spodnja definicija funkcije, ki pa ni pravilna. Drugih definicij v programu ni. Odgovori na
zastavljena vprašanja in popravi program na označenih mestih, da se bo prevedel in bo ohranil čim bolj enako semantiko
delovanja.
(define (foo a)
 (let ([b (+ c 1)]
 [c (+ a 1)])
 (+ a b)))
Primer želenega delovanja:
> (foo 1)
4
> (foo 2)
6
> (foo 5)
12
1.) Kdaj javi Racket napako v zgornjem programu? Izberi:
a) med prevajanjem programa
b) med izvajanjem programa Y
2.) Kakšno napako javi Racket? Izberi:
a) spremenljivka nima definirane vrednosti Y
b) napaka v sintaksi
c) nedefinirana spremenljivka
d) prekoračitev velikosti sklada
e) prekoračitev velikosti kopice
f) neskončna rekurzija
3.) Popravi program na označenih mestih:
(define (foo a)
 (_______([b _______________]
 [c (+ a 1)])
 _________))

 (define (foo a)
 (let ([c (+ a 1)]
       [b (+ c 1)])
 (+ a b)))



9. Označi trditve, ki so resnične!
a) funkcijsko programiranje je bolj primerno od objektno-usmerjenega, kadar v vseh funkcijah uporabljamo isti
podatkovni tip, ki ga lahko razširimo s spremembo definicije na enem samem mestu N
b) makro razširitev se izvede po statični analizi in pred prevajanjem Y
c) statično preverjanje lahko preverja tudi semantična pravila (poleg sintaktičnih) Y
d) programski jezik, ki izvaja dinamično preverjanje, potrebuje manj delovnega spomina in procesorskega časa N
e) statično preverjanje omogoča lažje prototipiranje programske kode N
f) sistem tipov v jeziku Racket preverja ustavljivost in je poln  Y
g) makro razširitev se izvede po prevajanju in pred izvajanjem programa N
h) JAIS je pretežno statično tipiziran jezik N
i) C++ sprejme (uspešno prevede) več programov kot Racket (če zanemarimo razlike v sintaksi in primerjamo samo
semantiko) N
j) JAIS je pretežno dinamično tipiziran jezik Y
k) objektno-usmerjeno programiranje je bolj primerno od funkcijskega, kadar pogosto razširjamo množico
podatkovnih tipov Y


. NALOGA (15 točk)
Odgovori na naslednja vprašanja, povezana z evalvacijo:
a) Kdaj se evalvira klic funkcije?
Runtime?
 A function call is evaluated when the program execution reaches the point where the function is called. This is typically during the runtime phase of a program.
b) Med koraki evalvacije funkcijskega klica oblike e0 (e1, … , en) so koraki:
I. evalviraj e0 v fun x0 (x1 : t1, … , xn : tn) = e
II. evalviraj argumente e1, ..., en v vrednosti v1, ..., vn
III. evalviraj telo e v rezultat funkcije, pri čemer preslikaj x1 v v1, ..., xn v vn
in v okolje, v katerem se izvede telo e, naj vsebuje tudi ime x0
Razloži tretji korak od zgornjih – kaj je njegov pomen in namen?

The third step in the evaluation of a function call e0 (e1, … , en) is the actual execution of the function body e with the arguments x1 to xn mapped to the evaluated values v1 to vn. This step is crucial as it is where the function performs its intended task. The environment in which the function body e is executed also contains the function name x0. This is important in cases where the function is recursive, i.e., it calls itself. The function name x0 in the environment allows the function to refer to itself within its body.
c) Kaj je to zakasnitvena funkcija?
A delay function is a function that postpones the evaluation of an expression until its value is needed. It is used in languages that support lazy evaluation, where expressions are not evaluated until their results are required. This can improve performance by avoiding unnecessary computations. In Racket, the delay function is used to create a delayed expression, and the force function is used to evaluate a delayed expression when its value is needed.


2. NALOGA (15 točk)
Določi podatkovni tip naslednji funkciji:
fun tip a (b, c) =
 case (b, a) of
(_, (SOME c)::x) => x
 | _ => tl a

 'a option list * 'b * 'c -> 'a option list

 they arent curried because they arent like a b c


NALOGA (25 točk)
V jeziku SML sta podani funkciji skip (izpiše vsak drugi element seznama) in sum2 (izračuna
seznam seštetih sosednjih parov elementov):
fun skip seznam =
 case seznam of
[] => []
 | [a] => []
 | _::drugi::rep => drugi::(skip rep)
STRAN 2 / 2
OBRNI!
fun sum2 seznam =
 case seznam of
[] => []
 | [a] => [a]
 | prvi::drugi::rep => (prvi+drugi)::(sum2 (drugi::rep))
Naloga:
a) Posploši obe funkciji (refaktoriziraj programsko kodo) v eno samo funkcijo višjega reda z
imenom splosna, ki jo ustrezno parametriziraj.
b) Zapiši delni aplikaciji funkcije splosna, imenovani skip_new in sum2_new, ki delujeta
enako kot podani funkciji.
c) Komentiraj možne načine delnih aplikacij.


fun splosna f g h seznam =
  case seznam of
    [] => []
  | [a] => [h a]
  | prvi::drugi::rep => f prvi drugi :: (splosna f g h (g prvi drugi rep))

val skip_new = splosna (fn _ drugi => drugi) (fn _ _ rep => rep) (fn _ => [])
val sum2_new = splosna (op +) (fn _ drugi rep => drugi::rep) (fn a => [a])


4. NALOGA (25 točk)
a) V programskem jeziku Racket definiraj konstrukt (struct) drevo, ki ima komponente left
(levo poddrevo), value (vrednost v vozlišču) in right (desno poddrevo).
S tem konstruktom lahko zapišemo drevo kot vgnezdeno strukturo, v kateri »null«
predstavlja prazno poddrevo, npr:
(define example (tree null 3 (tree null 4 null)))
b) Napiši funkcijo (imbalance tree n), ki kot vhodni argument sprejme drevo in celo število
n. Funkcija naj za celo drevo rekurzivno preveri, ali za vsako vozlišče velja, da je razlika v
višinah poddreves enaka največ n.
c) Zapiši različne primere funkcijskih klicev, od katerih ima vsak drugačno število evalvacij
spremenljivke n.
d) Uporabi funkciji za zakasnitev in sprožitev, da optimiziraš število evalvacij spremenljivke n.

a)
(struct tree (left value right))

b)
(define (height t)
  (if (tree? t)
      (+ 1 (max (height (tree-left t)) (height (tree-right t))))
      0))

(define (imbalance t n)
  (if (tree? t)
      (and (<= (abs (- (height (tree-left t)) (height (tree-right t)))) n)
           (imbalance (tree-left t) n)
           (imbalance (tree-right t) n))
      #t))

c)
(define example (tree null 3 (tree null 4 null)))

(imbalance example 0) ; returns #f
(imbalance example 1) ; returns #t
(imbalance example 2) ; returns #t

d)
(define (imbalance t n)
  (define delayed-n (delay n))
  (define (helper t)
    (if (tree? t)
        (and (<= (abs (- (height (tree-left t)) (height (tree-right t)))) (force delayed-n))
             (helper (tree-left t))
             (helper (tree-right t)))
        #t))
  (helper t))



5. NALOGA (20 točk)
V splošnem velja:
𝑒
𝑥 = ∑
𝑥
𝑛
𝑛!
∞
𝑛=0
V programskem jeziku Python izberi mehanizem, ki po principu lene evalvacije vrača elemente
neskončnega zaporedja in z njim implementiraj zaporedje elementov, ki predstavljajo izboljšane
približke izraza 𝑒
𝑥
:
[1, ∑
𝑥
𝑛
𝑛!
1
𝑛=0
= 1 + 𝑥, ∑
𝑥
𝑛
𝑛!
2
𝑛=0
= 1 + 𝑥 +
𝑥
2
2!
, … , ∑
𝑥
𝑛
𝑛!
𝑘
𝑛=0
, … ]
(vsak nadaljnji (k-ti) element zaporedja torej predstavlja obstoječo vsoto z še enim dodatnim
členom 𝑥
𝑘
𝑘!
)

def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n-1)

def e_approx(x):
    sum = 0
    n = 0
    while True:
        sum += x**n / factorial(n)
        yield sum
        n += 1


approximations = e_approx(1)
print(next(approximations))  # prints 1.0
print(next(approximations))  # prints 2.0
print(next(approximations))  # prints 2.5
# and so on...


1. NALOGA
Denimo, da sami definirate lasten programski jezik, ki ima trden in ustavljiv sistem tipov. Odgovori na naslednja
vprašanja:
a) Naštej vsaj tri stvari (na podlagi izkušenj pri programskih jezikih, ki smo jih uporabljali pri predmetu), ki jih pri
implementaciji tega novega programskega jezika moramo definirati v okviru definicije semantike jezika.

sitaksa, tipizator in pravila za evaluacijo?, makri
b) Na kratkem primeru programa (ki ga zapiši v sintaksi poljubnega jezika ali psevdokodi) podaj primer lažno
negativnega (false positive) programa in utemelji lažno negativnost.
c) Podaj primere treh obstoječih programskih jezikov, ki imajo trden in ustavljiv tipizator in jih razdeli med implicitno
in eksplicitno tipizirane jezike

b)
#lang racket

(define (add-or-concat a b)
  (if (and (number? a) (number? b))
      (+ a b)
      (if (and (string? a) (string? b))
          (string-append a b)
          (error "Invalid arguments"))))
e ; This will work
(add-or-concat 1 2) ; This will also work
(add-or-concat 1 "world!") ; This will cause an error in runtime


2. NALOGA
V programskem jeziku SML so podani trije programi (funkcije f1, f2 in f3), ki sprejmejo seznam elementov in
izračunajo:
 f1: vsoto dolžin nizov v seznamu,
 f2: vsoto dolžin podseznamov, ki so elementi seznama,
 f3: vsoto kvadratov elementov seznama.
a) Zapiši funkcijo višjega reda, ki posplošuje zgornje tri funkcije. Pri njeni definciji uporabi znane funkcije višjega reda
(map, filter, fold), če je to primerno, in uporabi currying.
b) Zapiši delne aplikacije posplošene funkcije v nove funkcije f1plus, f2plus in f3plus, ki izvajajo enake naloge kot
njihovi izvorni pari.

(* Higher-order function *)
fun higherOrderFunc f g = foldl (fn (x, acc) => acc + f (g x)) 0

(* Partially applied functions *)
val f1plus = higherOrderFunc String.length (fn x => x)
val f2plus = higherOrderFunc length (fn x => x)
val f3plus = higherOrderFunc (fn x => x * x) (fn x => x)

4. NALOGA
V programskem jeziku Racket definiraj konstrukt (struct) drevo, ki ima komponente id (unikatna oznaka vozlišča -
niz), left (levo poddrevo), value (vrednost v vozlišču) in right (desno poddrevo).
S tem konstruktom lahko zapišemo drevo kot vgnezdeno strukturo, v kateri »null« predstavlja prazno poddrevo.
Denimo, da s tem konstruktom zapisujemo binarna iskalna drevesa, kot je npr:
(define example
 (tree "koren"
 (tree "l" (tree "ll" null
 2
(tree "lld" null 3 null))
 5
 (tree "ld" null 7 null))
 10
 (tree "d" null
 15
 (tree "dd" null
 17
(tree "ddd" null 19 null)))))
Na opisanem drevesu želimo zaporedno iskanje več elementov v podanem seznamu, za kar bomo uporabili poisci.
Nalogi:
a) Napiši funkcijo poisci, ki sprejme seznam elementov in vrne seznam logičnih vrednosti, ki ustrezajo temu, ali
soležni element iz vhodnega seznama obstaja v drevesu. Funkcija naj uporablja princip memoizacije. Memoizacijo
naj izvaja tako, da ob vsakem iskanju nekega elementa (npr. elementa 6 v spodnjem primeru) shrani v seznam
rešitev vse elemente na poti (na poti v spodnjem primeru sta elementa 5 in 7, ne pa tudi 2 in 3). Ob kasnejšem
iskanju npr. elementa 7, ga funkcija vrne iz seznama rešitev in se ne išče po drevesu. Za vsak najden element naj
funkcija vrne ime vozlišča, v katerem je vrednost elementa bila najdena (oziroma null, če elementa ni v drevesu).
Imenu vozlišča naj bo pripeta zvezdica, če je vrednost najdena v seznamu rešitev (memoizacija). Primer klica te
funkcije na zgornjem primeru je:
> (poisci example (list 6 15 18 17 7 2))
'(null "d" null "dd*" "ld*" "ll")
b) Zakaj uporaba principa memoizacije na zgornjem primeru ni smiselna?


#lang racket

(define-struct tree (id left value right))

(define example
  (make-tree "koren"
             (make-tree "l" 
                        (make-tree "ll" null 2 (make-tree "lld" null 3 null))
                        5
                        (make-tree "ld" null 7 null))
             10
             (make-tree "d" null
                        15
                        (make-tree "dd" null
                                   17
                                   (make-tree "ddd" null 19 null)))))

(define memo (make-hash))

(define (poisci tree elements)
  (map (lambda (element) (search tree element)) elements))

(define (search tree element)
  (cond
    [(hash-ref memo element #f) => (lambda (x) (string-append x "*"))]
    [(null? tree) (hash-set! memo element "null") "null"]
    [(= element (tree-value tree)) (hash-set! memo element (tree-id tree)) (tree-id tree)]
    [(< element (tree-value tree)) (search (tree-left tree) element)]
    [else (search (tree-right tree) element)]))

(poisci example (list 6 15 18 17 7 2))

The reason why memoization is not useful in this case is because each search starts from the root of the tree, so the paths of different searches don't overlap much. Therefore, the probability of reusing the results of previous searches is low.

5. NALOGA
V programskem jeziku Python 3 napišite generator poštevanke. Generator naj kot argumente sprejme število
zahtevanih večkratnikov vsakega števila (podan argument z vrednostjo n pomeni: »generiraj 1-, 2-, … n- kratnik
števila) in seznam števil, katerih večkratnike bomo generirali.
Tako naj generator postevanka(10,[2,3]) po vrsti generira 1-, 2- do 10-kratnike števil 2 in 3. Vsak rezultat naj bo
oblike (število, faktor, zmnožek). V zgornjem primeru bi rezultat torej bil: (2,1,2), (2,2,4), (2,3,6),
..., (3,10,30).
Nalogo rešite na dva načina:
a) z uporabo generatorske funkcije in
b) z uporabo generatorskega izraza

def postevanka(n, numbers):
    for number in numbers:
        for i in range(1, n+1):
            yield (number, i, number*i)

# Test the generator
for result in postevanka(10, [2, 3]):
    print(result)


def postevanka(n, numbers):
    return ((number, i, number*i) for number in numbers for i in range(1, n+1))

# Test the generator
for result in postevanka(10, [2, 3]):
    print(result)

1. NALOGA
Odgovori na naslednja vprašanja:
a) Definirajte sintakso, semantiko in podatkovni tip funkcije hd v jeziku SML.
Syntax: hd list
Semantics: Returns the first element of list. If list is empty, it raises the List.Empty exception.
Type: 'a list -> 'a. This means it takes a list of elements of any type 'a and returns an element of the same type 'a.

b) Določite podatkovni tip naslednje funkcije:
fun mf x y =
 case x y of
 NONE => (fn y => [])
('a -> 'b option) -> 'a -> 'c -> 'd list

c) Kaj je to funkcijska ovojnica?
pri deklaraciji funkcije torej ni dovolj, da shranimo le programsko kodo funkcije,
temveč je potrebno shraniti tudi trenutno okolje
• FUNKCIJSKA OVOJNICA = koda funkcije + trenutno okolje 

d) Kaj je to šibko tipiziranje?
Weak typing means that the type system of a programming language allows more flexibility with how data types can be interchanged. This can lead to errors such as unintended type conversions, accessing memory locations that are not in use, etc. Examples of weakly typed languages include JavaScript and PHP.

Tipizator ki omogoca casting na spremenljivke

2. NALOGA
V programskem jeziku SML napiši naslednji program:
a) Definiraj rekurzivni polimorfni podatkovni tip drevo, ki predstavlja binarno drevo. Vsako
notranje vozlišče naj (poleg povezave na levo in desno poddrevo) hrani vrednost enega
podatkovnega tipa ('a), listi pa lahko hranijo vrednost drugega podatkovnega tipa ('b).
b) Napiši program, ki sprejme primer opisanega drevesa in vrne terko (a, b), kjer a in b
predstavljata število elementov tipa 'a in število elementov tipa 'b.
c) Denimo, da želimo napisati program, ki preveri, ali sta 'a in 'b isti podatkovni tip. Ali to
lahko naredimo? Obrazloži.

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree | Leaf of 'b

fun count_elements (Node (value, left, right)) =
    let
        val (a1, b1) = count_elements left
        val (a2, b2) = count_elements right
    in
        (a1 + a2 + 1, b1 + b2)
    end
  | count_elements (Leaf value) = (0, 1)

In SML, you cannot write a program that checks if 'a and 'b are the same data type. This is because SML is statically typed, which means that types are checked at compile time, not at runtime. Therefore, you cannot write a function that takes two values and returns whether they are the same type, because the types of the values are not known at runtime. Furthermore, SML's type system is strong, which means that it does not allow implicit type conversions. Therefore, you cannot compare values of different types or use a value of one type as if it were another type.

3. NALOGA
V programskem jeziku SML sta podana naslednji modul in podpis:
structure Logika :> LogSig1 =
struct
 type vrednost = bool * int
 exception Napaka
 fun izdelaj (x,y) = if y < 10 then (x,y) else raise Napaka
 fun obdelaj (x,y) = (not x, if y mod 2 = 0 then y-1 else y+1)
end

signature LogSig1 =
sig
 type vrednost
 val izdelaj : vrednost -> vrednost
 val obdelaj : vrednost -> vrednost
end
Odgovori na naslednja vprašanja:
a) Ali je podpis LogSig1 skladen z modulom?
b) Kaj se zgodi ob klicu funkcij izdelaj in obdelaj?
The izdelaj function takes a tuple (x, y) of type vrednost, checks if y is less than 10, and if so, returns the vrednost tuple (x, y). If y is not less than 10, it raises the Napaka exception. The obdelaj function takes a tuple (x, y), negates x, and if y is even, it subtracts 1 from y, otherwise it adds 1 to y. It then returns the tuple (not x, new_y).
c) S čim je potrebno dopolniti kodo za zagotovitev programov v modulu?
signature LogSig1 =
sig
 type vrednost
 exception Napaka
 val izdelaj : vrednost -> vrednost
 val obdelaj : vrednost -> vrednost
end


4. NALOGA
Napiši naslednjo programsko kodo v programskem jeziku Racket:
1. S spremenljivim seznamom (mutable list) predstavi igralno površino za igro križcev ("x") in
krožcev ("o"). Za ta namen uporabi seznam vgnezdenih seznamov (vsak notranji seznam
predstavlja eno vrstico), ki naj bo definiran globalno.
2. Napiši funkcijo oblike:
(poteza znak koordinate)
kjer je znak oznaka igralca, ki je naslednji na potezi (torej "x" ali "o"), koordinate pa so podane
v obliki parov vrstica-stolpec in jih je lahko več. Primer klica funkcije:
(poteza "x" (cons 3 1) (cons 1 1) (cons 2 3)).
Funkcija naj ustrezno spremeni igralno površino tako, da na podane koordinate doda
izmenjujoče poteze igralcev. V zgornjem primeru torej: na prvo koordinato (cons 3 1) doda
"x", na naslednjo koordinato (cons 1 1) doda "o" in na zadnjo koordinato (cons 2 3) doda
ponovno "x". Funkcija naj se ne ukvarja s tem, ali je polje na igralni plošči že zasedeno ali ne.


#lang racket

(define game-board (list (list #f #f #f) (list #f #f #f) (list #f #f #f)))

(define (list-set lst idx val)
  (if (zero? idx)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (sub1 idx) val))))

(define (poteza mark coordinates)
  (for-each (lambda (coordinate)
              (let* ((x (car coordinate))
                     (y (cadr coordinate))
                     (row (list-ref game-board x)))
                (set! game-board (list-set game-board x (list-set row y mark))))
              (set! mark (if (equal? mark "x") "o" "x")))
            coordinates))

(poteza "x" (list (list 0 0) (list 1 1) (list 2 2)))

game-board


1. NALOGA
Odgovori na naslednja vprašanja:
a) Definirajte sintakso, semantiko in omejitve ukaza val v jeziku SML.
Syntax: val identifier = expression
Semantics: It evaluates the expression and binds the result to identifier.
Constraints: The identifier must be a valid SML identifier and it must not have been previously defined in the same scope. The expression must be a valid SML expression.
b) Določite podatkovni tip naslednje funkcije:
fun mf x y =
 case (x,y) of
 (a, _) => a [3]

((int list -> 'a) * 'b) -> 'a
c) Na kakšne vse načine lahko optimiziramo funkcijske ovojnice v programskih jezikih?
Inlining: If the function is small, it can be inlined to avoid the overhead of a function call.
Dead code elimination: If the closure does not use some of the captured variables, they can be removed.
Lambda lifting: If a closure is only used in one place, it can be replaced with a function.
d) Pojasni težavo pri istočasnem ugotavljanju trdnosti, polnosti in ustavljivosti sistema za
preverjanje pravilnosti tipov.
The problem with simultaneously determining soundness, completeness, and decidability of a type system is that these properties are often at odds with each other:

Soundness means that if a program type checks, it will not produce any type errors at runtime.
Completeness means that if a program will not produce any type errors at runtime, it type checks.
Decidability means that there is an algorithm that can determine whether any program type checks.
However, it's impossible for a type system to be both sound and complete for all programs due to the undecidability of the halting problem. If a type system is sound and complete, it's not decidable. If it's decidable, it can't be both sound and complete.


2. NALOGA
Podana je naslednja funkcija process:
fun process x [] = [x]
 | process x (y::l) =
 if x < y then x::y::l
 else y::(process x l)
Naloge:
a) Opiši, kaj dela podana funkcija.
The function process takes an integer x and a list of integers. It inserts x into the list at the correct position to keep the list sorted in ascending order. If x is less than the first element of the list, it is inserted at the beginning. Otherwise, the function is called recursively on the rest of the list.

b) Podano funkcijo process je možno podati tudi z uporabo naslednje funkcije, če le pravilno
opredelimo njene argumente:
fun process2 x = foldr step [x]
Zakaj ima funkcija process2 manj argumentov kot prva?
The function process2 has fewer arguments because it uses foldr, which encapsulates the recursive pattern that process uses. foldr takes a binary function (in this case step), a starting value (in this case [x]), and a list to fold over. The list argument is not explicitly mentioned in the definition of process2 because it is an argument to foldr.

c) Podaj ustrezno definicjo argumenta step.
fun step y xs =
    if x < y then x::y::xs
    else y::(process x xs)


3. NALOGA
Na predavanjih smo implementirali funkcijo v jeziku Racket, ki uporablja memoizacijo za izračun
Fibonaccijevih števil. Denimo, da implementiramo funkcijo (potenca osnova eksponent), kot sledi:
potenca(osnova, eksponent) = {
1, č𝑒 𝑒𝑘𝑠𝑝𝑜𝑛𝑒𝑛𝑡 = 0
𝑜𝑠𝑛𝑜𝑣𝑎 ∗ 𝑝𝑜𝑡𝑒𝑛𝑐𝑎(𝑜𝑠𝑛𝑜𝑣𝑎, 𝑒𝑘𝑠𝑝𝑜𝑛𝑒𝑛𝑡 − 1), 𝑠𝑖𝑐𝑒𝑟
Če nadgradimo opisano funkcijo z memoizacijo, se izkaže, da ne dosežemo pohitritve. Odgovori:
a) Razloži, zakaj ne pride do pohitritve.
b) Napiši novo verzijo funkcije potenca, ki za argument osnova uporablja privzeto vrednost 2, za
eksponent privzeto vrednost 10, eksponent pa lahko naslavlja tudi s ključno besedo #:exp.
c) Podaj 5 različnih primerov klicev funkcije iz naloge b). Pri tem spreminjaj vrednosti argumentov
in njihov vrstni red. Za vsak klic podaj odgovor.

(define (potenca #:osnova [osnova 2] #:exp [exp 10])
  (if (zero? exp)
      1
      (* osnova (potenca #:osnova osnova #:exp (sub1 exp)))))

(potenca) ; Returns 1024, because the default values are 2 for the base and 10 for the exponent
(potenca #:osnova 3) ; Returns 59049, because the base is 3 and the default exponent is 10
(potenca #:exp 5) ; Returns 32, because the default base is 2 and the exponent is 5
(potenca #:osnova 3 #:exp 3) ; Returns 27, because the base is 3 and the exponent is 3
(potenca #:exp 3 #:osnova 3) ; Returns 27, because the base is 3 and the exponent is 3, order doesn't matter


1. NALOGA (20 točk)
Določi podatkovni tip naslednjim funkcijam:
a) fun prva (a, b) c =
 a::c

(a' list * 'b) -> a' list -> a' list
b) fun druga (a, b) c =
 {a=b andalso c, b=c, c=a}

('a * bool) -> bool -> {a=bool, b=bool, a='a}
c) fun tretja (a, b, c) =
 ([c a, c b], c)

(a' * 'b * ('a -> 'b)) -> ('a list * ('a -> 'b))
d) fun cetrta [a, b, c] =
 let val a = 3
 val b = 4
 in
 c a = c b
 end

('a * 'a * (int -> 'b)) list -> bool

3. NALOGA (20 točk)
Z uporabo rekurzivnega ujemanja vzorcev napiši funkcijo s podatkovnim tipom:
fn : (int * int) option list -> int option list
Funkcija naj deluje tako, da vhodni seznam preslika v izhodnega, in sicer:
 če za opcijo terke v vhodnem seznamu SOME (a,b) velja a>b, potem naj izhodni seznam
na tem mestu vsebuje element SOME (a+b);
 če za opcijo terke v vhodnem seznamu SOME (a,b) velja a<=b, potem naj izhodni seznam
na tem mestu vsebuje element NONE;
 če je opcija v vhodnem seznamu enaka NONE, je ta element v izhodnem seznamu izpuščen.

fun moja [] = []
  | moja (NONE :: xs) = moja xs
  | moja (SOME (a, b) :: xs) = 
      if a > b then SOME (a + b) :: moja xs
      else NONE :: moja xs

4. NALOGA (20 točk)
Napiši funkcijo, ki uporablja repno rekurzijo in deluje tako, da vsaki drugi element seznama (prvi,
tretji, …) poveča za vrednost 10. Primer delovanja:
- vsakdrug [4,5,6,7,8,9,8,7,6];
val it = [14,5,16,7,18,9,18,7,16] : int list

fun vsakdrug sez = 
  let
    fun helper ([], acc) = List.rev acc
      | helper ([x], acc) = helper([], x::acc)
      | helper (x1::x2::xs, acc) = helper(xs, x2::(x1 + 10)::acc)
  in
    helper(sez, [])
  end;

fun vsakdrug sez = 
  let
    fun helper ([], _, acc) = acc
      | helper (x::xs, true, acc) = helper(xs, false, acc @ [x + 10])
      | helper (x::xs, false, acc) = helper(xs, true, acc @ [x])
  in
    helper(sez, true, [])
  end;


1. NALOGA:
Podaj odgovore na naslednja vprašanja:
a) Zapiši sinatakso, semantiko, in postopek preverjanja pravilnosti tipov za n-mestno terko v
programskem jeziku SML.
In SML, an n-tuple is a collection of n values. The syntax for an n-tuple is (v1, v2, ..., vn), where v1, v2, ..., vn are the values in the tuple. The semantics of an n-tuple is that it's an ordered collection of values, and each value can be of a different type. The type of an n-tuple is (t1 * t2 * ... * tn), where t1, t2, ..., tn are the types of the values in the tuple. The type checking rule for an n-tuple is that if the types of v1, v2, ..., vn are t1, t2, ..., tn respectively, then the type of the tuple (v1, v2, ..., vn) is (t1 * t2 * ... * tn).


b) Določi podatkovni tip funkcije fun1:
datatype ('a, 'b) set = elem of 'a list * ('a, 'b) set | empty of 'b
fun fun1 (x, y) [z] =
 case y of
 elem ([nil], empty z) => x
 | _ => SOME z

('a option list * ('a, 'b) set) -> 'a list -> 'a option list


2. NALOGA:
Podani so podatkovni tip ('a, 'b) set in funkcije f1, f2 in f3:
datatype ('a, 'b) set = elem of 'a list * ('a, 'b) set | empty of 'b
fun f1 s =
 case s of
 elem (a, b) => List.length a + f1 b
 | empty _ => 0
fun f2 s =
 case s of
 elem (a, b) => (List.foldl op* 1 a) * f2 b
 | empty _ => 1
fun f3 s =
 case s of
 elem (a, b) => f3 b
 | empty b => b
a) Zapiši posplošeno funkcijo višjega reda (refaktoriziraj kodo), s katero lahko implementiraš delovanje
vseh treh podanih funkcij.
b) Poleg kode posplošene funkcije zapiši tudi delne aplikacije posplošene funkcije, s katerimi dosežemo
enako delovanje kot je delovanje podanih funkcij f1, f2 in f3.
c) Pri zapisih katerih delnih aplikacij posplošene funkcije (za f1, f2 in f3) imamo lahko težave z omejitvijo
vrednosti? Kako jih rešimo? 

fun fold_set f z s =
  case s of
    elem (a, b) => f a (fold_set f z b)
  | empty _ => z

val f1 = fold_set (fn a => fn b => List.length a + b) 0
val f2 = fold_set (fn a => fn b => List.foldl op* 1 a * b) 1
val f3 = fold_set (fn _ => fn b => b) 0