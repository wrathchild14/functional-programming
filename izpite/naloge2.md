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

