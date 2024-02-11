Podaj je naslednji izsek programske kode:

val x = 3
val w = fn _ => x
fun q x =
    let val x = 4
    in w
    end
val rez1 = q 30 15
fun w x = x + 1
val rez2 = q 31 14

1. Katerega od sistemov za doseg vrednosti uporablja SML?
Leksikalni doseg
2. Kaksna je vrednost spremenljivk rez1 in rez2, ce predpostavimo, da jezik uporablja dinamicni doseg?
rez1 = 4, rez2 = 5
3. Kaksna je vrednost spremenljivk rez1 in rez2, ce predpostavimo, da jezik uporablja leksikalni doseg?
rez1 = 3, rez2 = 5


V programskem jeziku Python želimo definirati dekorator flatten, ki bo rezultat funkcije, ki vrača
gnezdene sezname, pretvoril v sploščen seznam. Napišite definicijo tega dekoratorja (samo sploščevanje
definirajte z rekurzivno funkcijo)!
# Primer pred uporabo dekoratorja
def test():
 return [ 1, ['b','c'], [[1],[2]] ]
test()
>> [1, ['b', 'c'], [[1], [2]]]
# Primer po uporabi dekoratorja
@flatten
def test():
 return [ 1, ['b','c'], [[1],[2]] ]
test()
>>> [1, 'b', 'c', 1, 2]

```python
def flatten(func):
    def wrapper():
        def flatten_list(nested_list):
            result = []
            for i in nested_list:
                if isinstance(i, list):
                    result.extend(flatten_list(i))
                else:
                    result.append(i)
            return result

        return flatten_list(func())
    return wrapper

@flatten
def test():
    return [1, ['b', 'c'], [[1], [2]]]

print(test())  # Output: [1, 'b', 'c', 1, 2]
```

V programskem jeziku Racket napiši funkcijo z imenom prozi, ki sprejme funkcijo višjega reda f in zaporedno
številko elementa n. Funkcija naj oblikuje podatkovni tok, ki ga tvorijo obljube (promises), ki so izdelane s funkcijo
zakasnitve (delay). Obljube v toku čakajo na izračun vrednosti (f 1), (f 2), (f 3) itd. v tem zaporedju.
Izjema naj bo n-ti element v toku, ki pa naj bo v obliki že prožene obljube.
Predpostaviš lahko, da sta funkciji delay in force že implementirani.
Primer delovanja (za prikaz uporabljamo funkcijo (izpisi n tok) s predavanj, ki izpiše prvih n elementov toka tok:
Funkciji prozi podamo funkcijo (lambda (x) (* x 3)), ki izračuna trikratnik vhodne spremenljivke. Prozi
nam oblikuje tok obljub za izračun vrednosti 3 (f 1), 6 (f 2), 9 (f 3) itd. Zadnji parameter funkcije prozi
pove, kateri element naj bo že prožen – v prvem primeru je to 2. element, v drugem primeru pa 4.
> (izpisi 5 (prozi (lambda (x) (* x 3)) 2))
{#f . #<procedure>}
{#t . 6}
{#f . #<procedure>}
{#f . #<procedure>}
{#f . #<procedure>}
> (izpisi 5 (prozi (lambda (x) (* x 3)) 4))
{#f . #<procedure>}
{#f . #<procedure>}
{#f . #<procedure>}
{#t . 12}
{#f . #<procedure>}
Rešitev:
(define (prozi f n)
  (define (helper i)
    (cons (if (= i n)
              (cons #t (force (delay (f i))))
              (cons #f (delay (f i))))
          (lambda () (helper (+ i 1)))))
  (helper 1))


val u = 1
fun f v =
let
val u = v + 1
in
fn w => u + v + w
end
val u = 3
val g = f 4
val v = 5
val w = g 6



V okviru predavanj o zakasnjeni evalvaciji smo v programskem jeziku implementirali funkciji za zakasnitev
(delay) in sprožitev (force). Naloge:
a) V programskem jeziku SML po podobnem vzoru implementiraj funkciji za zakasnitev in sprožitev, ki
delujeta na način, kot ga prikazuje primer v nadaljevanju. Pri tem ustrezno definirajte tip thunk,
namesto spremenljivega para (mcons) pa uporabite referenco na terko.
- val x = mydelay (funk (fn () => 13+5));
val x = ref (false,funk fn) : (bool * int thunk) ref
- myforce x;
val it = 18 : int
- x;
val it = ref (true,rez 18) : (bool * int thunk) ref
- myforce x;
val it = 18 : int
- x;
val it = ref (true,rez 18) : (bool * int thunk) ref

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


b) Podaj podatkovna tipa funkcij mydelay in myforce.
mydelay : (unit -> 'a) -> unit
myforce : 'a thunk ref -> 'a
c) Zakaj je implementacija novega tipa thunk nujno potrebna v primerjavi s funkcijo v Racketu?
 The implementation of a new thunk type is necessary in SML because SML is a strictly evaluated language, unlike Racket which is a lazily evaluated language. In a strictly evaluated language, arguments to a function are evaluated before the function is called. So, we need a way to delay the evaluation of a function until it's actually needed. This is what the thunk type does.
d) Ali je možno strukturo obljube (promise) poenostaviti? Če da, kako? Na osnovi katerega mehanizma v
jeziku SML?
The structure of the promise could be simplified by removing the boolean flag and just storing the result directly in the thunk after it's computed. This is possible because of SML's mutable state, which allows us to update the thunk in place. However, this would make the thunk less general because it could only be used for computations that produce a result (not for computations that produce side effects or that don't terminate).


Generiranje (fraktalnih) števil Mandelbrotove množice poteka induktivno na naslednji način:
 začetna vrednost: 𝑍0 = 0 + 0𝑗 (kompleksna ničla)
 Induktivni korak: 𝑍𝑛+1 = (𝑍𝑛)
2 + 𝑐 (c je parameter, kompleksna konstanta, npr. c=0.1+0.2j)
Namig: V Pythonu s kompleksnimi števili delamo sintaktično enako kot z realnimi (uporabljamo tudi enake
operacije). Primer:
>>> stevilo1 = 6+5j
>>> stevilo2 = 4-3j
>>> stevilo1
(6+5j)
>>> stevilo1+stevilo2
(10+2j)
>>> abs(stevilo1+stevilo2)
10.19803902718557
Postopek generira števila, dokler ne doseže maksimalnega števila korakov (parameter maxstep) ali dokler ni
izpolnjen pogoj |𝑍𝑛| > 2, kar lahko v Pythonu preverimo z zapisom abs(Zn)>2. Naloge:
a) V programskem jeziku Python definirajte generatorsko funkcijo Mandelbrot(c, maxstep), ki
generira števila na zgoraj opisan način.
def Mandelbrot(c, maxstep):
    Zn = 0 + 0j
    for n in range(maxstep):
        if abs(Zn) > 2:
            break
        yield Zn
        Zn = Zn**2 + c
b) Zapišite primer uporabe, ki izpiše rezultate generatorja s parametroma 𝑐 = 0.2 + 0.7𝑗 in maxstep=100
z njihovo zaporedno številko, rezultatom in absolutno vrednostjo. Primer izpisa:
Zap.št. Rezultat Abs. vrednost
0 0j 0.000
1 (0.2+0.7j) 0.728
2 (-0.249+0.98j) 1.011
...
Namig: V jeziku Python lahko spremenljivke a, b, c izpišete z ukazom print(a,b,c).

for i, Zn in enumerate(Mandelbrot(0.2 + 0.7j, 100)):
    print(f"Zap.št. {i} Rezultat {Zn} Abs. vrednost {abs(Zn)}")

c) Ali bi lahko namesto generatorja uporabili generatorski izraz? Če da, ga zapišite, če ne, pa navedite
razlog.
It's not possible to use a generator expression for this task because the calculation of the next Zn depends on the current Zn, and this kind of stateful computation cannot be expressed with a generator expression. Generator expressions are more suited for stateless computations where each output can be computed independently from the others.


Podaj odgovore na naslednja vprašanja:
a) Zapiši sinatakso, semantiko, in postopek preverjanja pravilnosti tipov za n-mestno terko v
programskem jeziku SML.
In Standard ML (SML), an n-tuple is a fixed-size collection of n elements that can be of different types.
Syntax: The syntax for an n-tuple is (e1, e2, ..., en), where e1, e2, ..., en are expressions.
Semantics: An n-tuple represents a collection of n values. The order of the elements is significant.
Type checking: The type of an n-tuple is a product type, which is the Cartesian product of the types of its elements. The type of the tuple (e1, e2, ..., en) is (t1, t2, ..., tn), where t1, t2, ..., tn are the types of e1, e2, ..., en, respectively. The type checker verifies that each expression ei has type ti.

b) Določi podatkovni tip funkcije fun1:
datatype ('a, 'b) set = elem of 'a list * ('a, 'b) set | empty of 'b
fun fun1 (x, y) [z] =
 case y of
 elem ([nil], empty z) => x
 | _ => SOME z



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
fun splosna f g s =
  case s of
    elem (a, b) => f a (splosna f g b)
  | empty b => g b
b) Poleg kode posplošene funkcije zapiši tudi delne aplikacije posplošene funkcije, s katerimi dosežemo
enako delovanje kot je delovanje podanih funkcij f1, f2 in f3.
fun f1 s = splosna (fn a => fn b => List.length a + b) (fn _ => 0) s
fun f2 s = splosna (fn a => fn b => List.foldl op* 1 a * b) (fn _ => 1) s
fun f3 s = splosna (fn _ => fn b => b) (fn b => b) s
c) Pri zapisih katerih delnih aplikacij posplošene funkcije (za f1, f2 in f3) imamo lahko težave z omejitvijo
vrednosti? Kako jih rešimo?
For f1 and f2, we might run into issues with stack overflow for large inputs because of the recursive calls. This is because SML uses eager evaluation and does not automatically optimize tail recursion. We can solve this by manually implementing tail recursion with an accumulator:
fun f1 s =
  let
    fun helper acc s =
      case s of
        elem (a, b) => helper (acc + List.length a) b
      | empty _ => acc
  in
    helper 0 s
  end

fun f2 s =
  let
    fun helper acc s =
      case s of
        elem (a, b) => helper (acc * List.foldl op* 1 a) b
      | empty _ => acc
  in
    helper 1 s
  end


V programskem jeziku Racket napiši tok, ki je predstavljen s trimestno terko. Prva komponenta terke naj
vsebuje funkcijo za generiranje predhodnega elementa, druga komponenta naj hrani vrednost trenutnega
elementa in tretja komponenta naj hrani funkcijo za generiranje naslednjega elementa. Nalogi:
a) Napiši tok, ki se imenuje potence in se prične z vrednostjo 2. Vsaka vrednost prejšnjega elementa naj
bo za faktor 2 manjša, vsaka naslednja vrednost toka pa za faktor 2 večje.
b) Napiši makre prejsnji, trenutni in naslednji, ki poenostavijo dostop do elementov tega toka.
```
Primer:
> potence
'(#<procedure> 2 #<procedure>)
> (trenutni potence)
2
> (prejsnji potence)
'(#<procedure> 1 #<procedure>)
> (trenutni (prejsnji potence))
1
> (naslednji (naslednji (naslednji (naslednji potence))))
'(#<procedure> 32 #<procedure>)
> (trenutni (naslednji (naslednji (naslednji (naslednji potence)))))
32
```

(define (make-stream prev-fn current next-fn)
  (list prev-fn current next-fn))

(define (prev stream)
  ((first stream) stream))

(define (current stream)
  (second stream))

(define (next stream)
  ((third stream) stream))

(define potence
  (letrec ([potence (make-stream
                      (lambda (s) (make-stream prev (/ (current s) 2) next))
                      2
                      (lambda (s) (make-stream prev (* (current s) 2) next)))])
    potence))




Na predavanjih smo pokazali, kako v programskem jeziku Python "na roke" implementiramo currying.
a) Implementiraj dekorator z imenom curry3, ki za poljubno funkcijo z natanko tremi pozicijskimi
argumenti izpiše, koliko argumentov je bilo podano s pristopom currying. 

Primer: Denimo, da
dekorator pripišemo naslednji funkciji:
@curry3
def f(a,b,c):
 return a+b+c
Želimo, da naslednji klici funkcije izpišejo želeni podatek in šele nato podajo rezultat:
```
>>>f(1,2,3)
Curryied parametrov: 1
6
>>> f(1)(2,3)
Curryied parametrov: 2
6
>>> f(1,2)(3)
Curryied parametrov: 2
6
>>> f(1)(2)(3)
Curryied parametrov: 3
6
```
def curry3(func):
    count = 0
    def curried(*args):
        nonlocal count
        count += 1
        if len(args) < 3:
            return lambda *args2: curried(*(args + args2))
        else:
            print("Curried parametrov: ", count)
            count = 0
            return func(*args)
    return curried

@curry3
def f(a, b, c):
    return a + b + c

b) (splošno vprašanje) Denimo, da neko funkcijo implementiramo na dva načina (v poljubljem
funkcijskem programskem jeziku): (1) kot funkcijo, ki sprejema argumente kot terko in (2) kot funkcijo,
ki uporablja currying za sprejemanje argumentov. Kateri funkcijski klic bo načeloma deloval hitreje in
zakaj?

Generally, a function call that uses currying will be slower than a function call that takes all arguments at once. This is because each call to a curried function creates a new function that needs to be stored in memory, which takes time. In contrast, a function call that takes all arguments at once can be executed immediately. However, the difference in speed is usually negligible and would only be noticeable for very large numbers of function calls. The advantage of currying is that it allows for more flexible function calls and can make the code easier to read and understand.



Podaj odgovore na naslednja vprašanja (največ 2 povedi na vprašanje):
a) Ali v programskem jeziku Racket obstaja polimorfizem? Razloži.

 Yes, Racket supports polymorphism. It is a dynamically typed language, which means a function can accept arguments of any type and behave differently based on the type of the arguments it receives.

b) Pojasni, kako se prenašajo parametri v jeziku SML pri (1) definiciji funkcije in (2) pri funkcijskem klicu –
po vrednosti ali po imenu? Pojasni oba odgovora.

 In Standard ML (SML), parameters are passed by value. This means that when a function is defined or called, a copy of the actual parameter is made and passed to the function. Changes made to the parameter inside the function do not affect the original argument.

c) Katere izmed pojavitev spremenljivk v naslednji funkciji moramo nujno hraniti v funkcijski ovojnici?
Razloži:
```
(lambda (b) (+ (let ([a 2]) (- a))
 (- a)
 (let ([c b]) c)
 (let ([b c]) b)))
```

 In the given lambda function, the occurrences of variables a and b that are bound in the let expressions need to be stored in the closure. This is because they might be used later in the function after their binding expressions have exited scope.

d) Kaj je to sintaksno abstraktno drevo? Kakšno vlogo je igralo pri izdelavi interpreterja za JAIS?

 A syntax abstract tree (AST) is a tree representation of the abstract syntactic structure of source code. It played a crucial role in the creation of the JAIS interpreter as it is used to parse the source code into a structure that can be easily processed to generate the desired output or perform other analyses.


V programskem jeziku SML so podani opisi artiklov in seznam nakupov:
```
val r1 = {artikel="kruh", cena_enote=2.5, enot=4}
val r2 = {artikel="mleko", cena_enote=1.2, enot=3}
val r3 = {artikel="sir", cena_enote=3.4, enot=5}
val nakup = [("branko", [r1,r2]), ("natasa", [r2,r3]), ("branko", [r3])]
```

Pri vsakemu artiklu so podani: naziv artikla (npr. "kruh"), cena enote in število kupljenih enot. Strošek nakupa
artikla torej izračunamo kot produkt količin cena_enote in enot. Seznam nakupov podaja, kdo je kupil
katerega od artiklov (npr. branko je kupil artikla r1 in r2 itd.). Naloge:

a) Kakšen je podatkovni tip seznama nakup?

item = {artikel: string, cena_enote: real, enot: int}
nakup = (string * item list) list

b) Napiši funkcijo fun zapravil sez ime, ki sprejme seznam nakupov in ime kupca, ter vrne skupni
strošek nakupa. Pri implementaciji funkcije obvezno uporabi:
 rekurzivno gnezdenje vzorcev in
 vsaj eno od funkcij višjega reda map/filter/fold.
(rešitve, ki ne ustrezajo zgornjim pogojem so vredne do 10% točk naloge)

fun zapravil [] _ = 0.0
  | zapravil ((buyer, items)::rest) name =
    if buyer = name then
      List.foldl (fn ({cena_enote, enot, ...}, acc) => cena_enote * real enot + acc) 0.0 items
      + zapravil rest name
    else
      zapravil rest name
c) Kakšen je podatkovni tip funkcije zapravil? Pojasni posamezne komponente zapisanega
podatkovnega tipa.
((string * {artikel: string, cena_enote: real, enot: int} list) list) * string -> real


V programskem jeziku Racket napiši funkcijo z imenom funkcijski, ki sprejme neko funkcijo f in njen
začetni argument initial, vrne pa tok. Prvi element tega toka naj ima vrednost podanega začetnega
argumenta initial, vsak naslednji element toka pa rezultat funkcije na vhodu, ki je prejšnji element toka.
Primer:
# funkcija, ki jo bomo uporabili za delo s tokom – zamakne elemente seznama v levo
> (define (rotate sez) (append (cdr sez) (list (car sez))))
# definiramo tok x, podamo mu funkcijo rotate in začetni seznam
> (define x (funkcijski rotate (list 1 3 5 7 9 11)))
> x
'((1 3 5 7 9 11) . #<procedure>)
> (izpisi 8 x) # izpis prvih 8 elementov toka (funkcija s predavanj)
(1 3 5 7 9 11)
(3 5 7 9 11 1)
(5 7 9 11 1 3)
(7 9 11 1 3 5)
(9 11 1 3 5 7)
(11 1 3 5 7 9)
(1 3 5 7 9 11)
(3 5 7 9 11 1)

(define (funkcijski f initial)
  (stream-cons initial (funkcijski f (f initial)))) 


V programskem jeziku Python želimo implementirati funkcijo group, ki je posplošitev funkcije reduce/fold.
Funkcija group naj sprejema seznam parov oblike (ključ, vrednost) in naj grupira vse elemente, ki imajo
isti ključ. Ob grupiranju naj izvede agregacijo s podano funkcijo aggregator. Funkcija group naj ima torej
obliko:
def group(data, aggregator, initial_value):
kjer je data seznam vhodnih parov, agregator funkcija dveh argumentov (enakih kot jih ima funkcija
reduce/fold), initial_value pa začetna vrednost pri agregaciji (privzeta vrednost je 0). Naloge:
a) Z uporabo sintakse za sestavljenje sezname ustvari seznam 1000 naključnih terk. Ključi naj imajo zalogo
vrednosti [1..10], vrednosti pa zalogo vrednosti [0..100]. Uporabiš lahko funkcijo radint(a,b) iz
modula random, ki generira cela števila na intervalu [a..b]. Primer seznama (krajšega od 1000
elementov):
data = [(2, 1), (1, 1), (3, 0), (3, 1), (1, 5), (1, 0), (1, 0), (2, 10), (3, 7), (3, 3)]

data = [(random.randint(1, 10), random.randint(0, 100)) for _ in range(1000)]
b) Implementiraj funkcijo group, pri čemer uporabi samo funkcije map, filter in reduce.
def group(data, aggregator, initial_value=0):
    keys = set(map(lambda x: x[0], data))
    return [(key, reduce(aggregator, map(lambda x: x[1], filter(lambda x: x[0] == key, data)), initial_value)) for key in keys]
c) Podaj primere klicev funkcije group za izračun vsote (SUM), števila (COUNT), minimuma (MIN) in
maksimuma (MAX) grupiranih vrednosti. Pri klicu funkcije uporabi izključno anonimne funkcije. Primer
rezultatov za različne funkcije na seznamu iz zgornjega primera:
COUNT: [(1, 4), (2, 2), (3, 4)]
SUM: [(1, 6), (2, 11), (3, 15)]
MIN: [(1, 0), (2, 1), (3, 0)]
MAX: [(1, 5), (2, 10), (3, 7)]

# SUM
print(group(data, lambda x, y: x + y))

# COUNT
print(group(data, lambda x, y: x + 1, 0))

# MIN
print(group(data, min, float('inf')))

# MAX
print(group(data, max, float('-inf')))
d) Ali je možno implementirati tudi agregator AVG (povprečje vrednosti). Če da, podaj primer takšnega
klica funkcije group; če ne, razloži, zakaj ne.

def avg_aggregator(x, y):
    sum, count = x
    return sum + y, count + 1

# AVG
avg_data = group(data, avg_aggregator, (0, 0))
avg_data = [(key, sum / count) for key, (sum, count) in avg_data]
print(avg_data)



Krivulje Sierpinskega lahko narišemo s funkcijami, ki so podane spodaj (funkcije so zapisane v pythonovski
psevdokodi, ki ne vsebuje nepotrebnih podrobnosti, kot so izris krivulje). Funkcija Sierpinski sprejema pet
argumentov: level (celo število – integer) in štiri funkcije (imenovane fA, fB, fC in fD). Primer
def Sierpinski(level, fA, fB, fC, fD):
fA(level)
fB(level)
fC(level)
fD(level)
def A(level):
if (level > 0):
A(level - 1)
B(level - 1)
D(level - 1)
A(level - 1)
def B(level):
if (level > 0):
B(level - 1)
C(level - 1)
A(level - 1)
B(level - 1)
def C(level):
if (level > 0):
C(level - 1)
D(level - 1)
B(level - 1)
C(level - 1)
def D(level):
if (level > 0):
D(level - 1)
A(level - 1)
C(level - 1)
D(level - 1)
V jeziku SML lahko torej funkcijo Sierpinski pokličemo npr. s klicem:
val x = Sierpinski 5 A B C D;
Nalogi:
a) Zapiši implementacijo zgornjih petih funkcij (Sierpinski, A, B, C in D) v programskem jeziku SML. Pri
vseh funkcijah uporabi currying.

fun Sierpinski level fA fB fC fD = (fA level, fB level, fC level, fD level)

fun A level =
  if level > 0 then
    (A (level - 1), B (level - 1), D (level - 1), A (level - 1))
  else
    ()

fun B level =
  if level > 0 then
    (B (level - 1), C (level - 1), A (level - 1), B (level - 1))
  else
    ()

fun C level =
  if level > 0 then
    (C (level - 1), D (level - 1), B (level - 1), C (level - 1))
  else
    ()

fun D level =
  if level > 0 then
    (D (level - 1), A (level - 1), C (level - 1), D (level - 1))
  else
    ()
b) Za vsako od petih funkcij določi njen podatkovni tip
Sierpinski: (int -> 'a) -> (int -> 'a) -> (int -> 'a) -> (int -> 'a) -> 'a
A: int -> unit
B: int -> unit
C: int -> unit
D: int -> unit


V programskem jeziku Python implementiraj cenzoriranje rezultatov funkcij, ki vračajo sezname. Cenzoriranje
implementiraj z izdelavo dekoratorja funkcije, ki preveri ustreznost elementov v seznamu, ki ga funkcija vrača.
Ustreznost elementov naj dekorator preveri s podanim predikatom censor_pred. Kot rezultat pa naj
dekorator vrača par generatorjev, od katerih bo prvi generiral vse (necenzorirane) elemente seznama, drugi pa
filtrirane (cenzorirane) – torej samo tiste, ki ustrezajo predikatu.
Primer ogrodja rešitve:
@censor(censor_pred) # dekorator za cenzoriranje
funkcija_vraca_seznam(...) # funkcija, ki vrača seznam elementov
...
# klic dekorirane funkcije vrača par generatorjev
cenzoriran, necenzoriran = funkcija_vraca_seznam(...)

def censor(censor_pred):
    def decorator(func):
        def wrapper(*args, **kwargs):
            result = func(*args, **kwargs)
            return (x for x in result), (x for x in result if censor_pred(x))
        return wrapper
    return decorator


V programskem jeziku Racket je podan naslednji seznam, imenovan binarni:
(define binarni (mcons 1 (mcons 0 '())))
(set-mcdr! (mcdr binarni) binarni)
Naloge:
a) Za podani seznam definiraj funkcijo (blen binarni), ki vrne dolžino seznama.
(define (blen binarni)
  (if (null? binarni)
      0
      (+ 1 (blen (cdr binarni)))))
b) Za podani seznam definiraj funkcijo (bprint n binarni), ki izpiše prvih n elementov seznama.
(define (bprint n binarni)
  (if (> n 0)
      (begin
        (displayln (car binarni))
        (bprint (- n 1) (cdr binarni)))))
c) Kakšen seznam je seznam binarni?
The binarni list is a circular list that alternates between 1 and 0.
d) Kakšen je rezultat klica (bprint 4 binarni)?
The result of (bprint 4 binarni) is 1010.
e) Kakšen je rezultat klica (blen binarni)?
The result of (blen binarni) is infinite because binarni is a circular list
f) Ali bi lahko podoben seznam implementirali tudi v jeziku SML? Razloži na kratko, največ v eni povedi.
In SML, you cannot create circular lists like in Racket because SML is a strictly evaluated language, which means it does not support circular data structures.

Posplošena drevesa so drevesa, v katerih ima lahko vsako vozlišče poljubno število naslednikov. Predhodni
obhod drevesa (angl. preorder traversal of tree) izvedemo tako, da najprej obiščemo starša, nato pa obhod
rekurzivno ponovimo za vsakega od naslednikov. Naknadni obhod (angl. postorder traversal) deluje ravno
obratno: najprej rekurzivno obišče vse naslednike vozlišča in šele na koncu vozlišče samo.
Naloge:
a) Implementiraj modul Drevo (s strukturama signature in
structure). Vsako drevo naj bo implementirano tako, da vsa
vozlišča hranijo podatek istega tipa (vendar pa dve drevesi lahko
hranita podatke različnih tipov). V modul vključi definicijo
podatkovnega tipa in funkciji predhodni in naknadni).
signature TREE = 
sig
  type 'a tree
  val empty : 'a tree
  val leaf : 'a -> 'a tree
  val node : 'a * 'a tree list -> 'a tree
  val preorder : 'a tree -> 'a list
  val postorder : 'a tree -> 'a list
end

structure Tree : TREE = 
struct
  datatype 'a tree = Empty | Leaf of 'a | Node of 'a * 'a tree list

  val empty = Empty

  fun leaf x = Leaf x

  fun node (x, ts) = Node (x, ts)

  fun preorder Empty = []
    | preorder (Leaf x) = [x]
    | preorder (Node (x, ts)) = x :: List.concat (List.map preorder ts)

  fun postorder Empty = []
    | postorder (Leaf x) = [x]
    | postorder (Node (x, ts)) = List.concat (List.map postorder ts) @ [x]
end
b) Pri implementaciji podpisa (signature) smiselno določi način
abstrakcije oz. skrivanja implementacije.
The signature TREE abstracts the implementation of the tree. It only exposes the type 'a tree and the necessary functions, but not the underlying datatype.
c) Podaj primer, s katerim na svoji implementaciji ponazoriš uporabo
praznega drevesa in lista drevesa.
val t1 = Tree.empty
val t2 = Tree.leaf 5
d) S svojim podatkovnim tipom zapiši predstavitev drevesa s slike.
e) Kakšen rezultat vrneta funkciji predhodni in naknadni za drevo
s slike?



Potenčna množica množice S vsebuje njene vse možne podmnožice, vključno s prazno množico in celotno
množico S. V programskem jeziku Racket napiši funkcijo (potencna S), ki vrne potenčno množico množice
s. Pri tem lahko množico predstaviš kot seznam elementov, ki so med seboj različni.
Primer:
(potencna '(0 1 2 3))
'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3) (0) (0 3) (0 2) (0 2 3)
(0 1) (0 1 3) (0 1 2) (0 1 2 3))
(define (potencna s)
  (if (null? s)
      '(())
      (let* ((rest (potencna (cdr s)))
             (first (car s)))
        (append rest (map (lambda (x) (cons first x)) rest)))))


1. NALOGA:
Podaj odgovore na naslednja vprašanja (največ 2 povedi na vprašanje):
a) Zakaj pristop currying deluje počasneje, kot če uporabljamo funkcije z več argumenti?
- dodatni klici funkcij, vecja poraba pomnilnika, zapletenost implementacije
i would say, because functions in SML are passed by value and there wont be more calculations, in contrast to currying.

b) Zakaj je pomembno, da makro sistem uporablja leksikalni doseg?
- Predvidljivost, izolacija (marko ne more nenamerno spreminiti ali biti vplivan s spremenljivkami v okolju, kjer je klican), optimizacija (da kompajler ve, na katero vezavo se nanasa doloceno ime)

c) Čemu je namenjen postopek optimizacije funkcijskih ovojnic? Kako jih optimiziramo, kaj s tem
pridobimo?
- Optimizacija funkcijskih ovojnic običajno vključuje odstranjevanje nepotrebnih spremenljivk iz ovojnice. S tem pridobimo večjo učinkovitost in zmogljivost, saj se zmanjša poraba pomnilnika in potencialno tudi čas izvajanja. Poleg tega lahko optimizacija funkcijskih ovojnic izboljša tudi čitljivost in vzdrževanje kode, saj odstranjuje nepotrebne spremenljivke in poenostavlja strukturo funkcij.

d) Določi podatkovni tip naslednji funkciji:
fun a b c =
 case b of
 {d=u, e=v} => if (u=1) andalso (v=false)
 then 3.14
 else c b
- fun a = {d:int, e:bool} -> ({d:int, e:int} -> real) -> real

2. NALOGA:
V programskem jeziku SML zapiši funkcijo, ki prejme seznam celih števil. Funkcija naj nato izračuna produkt
zaporednih elementov v seznamu, torej produkt prvega in drugega elementa, produkt drugega in tretjega
elementa itn. Rezultat funkcije naj bo torej seznam produktov (celih števil), ki je za 1 element krajši od
vhodnega seznama.
Implementacijo implementiraj izključno s funkcijami višjega reda (map/filter/fold).
Primer delovanja:
- produkt [4,3,2,7,5];
val it = [12,6,14,35] : int list
```
fun product_of_pairs xs =
  let
    val shifted_xs = tl xs
  in
    ListPair.map (fn (x, y) => x * y) (xs, shifted_xs)
  end;

fun product_of_pairs_more xs =
  let
    val ys = tl xs
    val pairs = ListPair.zip (xs, ys)
  in
    List.map (fn (x, y) => x * y) pairs
  end;
```

3. NALOGA:
V programskem jeziku Racket je podano ogrodje interpreterja za jezik JAIS, kot smo to naredili na predavanjih:
(struct konst (int) #:transparent) ; konstanta; argument je stevilo
(struct sestej (e1 e2) #:transparent) ; e1 in e2 sta izraza
(struct mnozi (e1 e2) #:transparent) ; e1 in e2 sta izraza
(struct bool (b) #:transparent) ; b ima lahko vrednost true or false
(define (jais e)
 (cond [(konst? e) e] ; vrnemo izraz v ciljnem jeziku
 [(bool? e) e]
 [(sestej? e) <<< tukaj sledi koda za implementacijo seštevanja >>>]
 [(mnozi? e) <<< tukaj sledi koda za implementacijo množenja >>>]
 ))
V zgornjem interpreterju sta implementirana seštevanje in množenje (predpostavi, da je koda že napisana –
implementiranje ni del izpitne naloge). V jezik interpreterja lahko torej prevedemo izračun izraza 2×3+1 (kar
znaša 7) v
> (jais (sestej (mnozi (konst 2) (konst 3)) (konst 1)))
(konst 7)
Denimo pa, da želimo interpreterju ukazati, naj upošteva drugačno prioriteto operatorjev pri izračunu izraza
2×3+1, in sicer naj izračuna 2×(3+1), kar znaša 8. To bi lahko sicer opravili s klicem:
> (jais (mnozi (konst 2) (sestej (konst 3) (konst 1)))
(konst 8)
vendar pa želimo ubrati drugačno pot, kot je opisano v nadaljevanju.
Implementiraj konstrukt (prioriteta izraz), ki sprejme izraz za seštevanje (sestej e1 e2), katerega
sintaksno drevo ima natanko tri konstante, in naj v njih obrne zaporedje prednosti izračunov. Torej:
 če je izraz oblike (sestej (mnozi e3 e4) e2), naj bo rezultat (mnozi e3 (sestej e4 e2))
 če je izraz oblike (sestej e1 (mnozi e3 e4)), naj bo rezultat (mnozi (sestej e1 e3) e4)
Na vse drugačne oblike vhodnega izraza naj konstrukt prioriteta nima vpliva. Primer uporabe:
> (jais (prioriteta (sestej (mnozi (konst 2) (konst 3)) (konst 1))))
(konst 8)

(struct prioriteta (e) #:transparent) ; e is an expression

(define (jais e)
 (cond [(konst? e) e] ; return the expression in the target language
       [(bool? e) e]
       [(sestej? e) <<< code for addition implementation goes here >>>]
       [(mnozi? e) <<< code for multiplication implementation goes here >>>]
       [(prioriteta? e)
        (let ([e (prioriteta-e e)])
          (cond [(and (sestej? e) (mnozi? (sestej-e1 e)))
                 (mnozi (mnozi-e1 (sestej-e1 e)) (jais (sestej (mnozi-e2 (sestej-e1 e)) (sestej-e2 e))))]
                [(and (sestej? e) (mnozi? (sestej-e2 e)))
                 (mnozi (jais (sestej (sestej-e1 e) (mnozi-e1 (sestej-e2 e)))) (mnozi-e2 (sestej-e2 e)))]
                [else e]))]))

4. NALOGA:
V programskem jeziku Python 3 napišite dekorator enkrat, ki dovoli le en klic funkcije z enakimi argumenti.
Če jo z enakimi argumenti pokličemo še enkrat, naj se izpiše napaka ali proži izjema. Tako npr. prvi klic
f(2,3) uspe, drugi klic f(2,3) pa ne. Omejite se le na pozicijske argumente. 

def enkrat(f):
  cache = set()
  def wrapper(*args):
    if args in cache:
      raise ValueError(f"Function {f.__name__} has already been called with these arguments: {args}")
    else:
      cache.add(args)
      return f(*args)
  return wrapper



1. NALOGA:
V programskem jeziku SML je podana naslednje funkcija:
fun nekaj a b c d e f = a (b c) (d e);
a) Določi podatkovni tip zgornje funkcije.
('a -> 'b -> 'c -> 'd) -> 'a -> ('e -> 'b) -> 'e -> 'c -> 'd
b) Zapiši anonimno funkcijo, ki je semantično ekvivalentna podani funkciji nekaj.
fn a => fn b => fn c => fn d => fn e => fn f => a (b c) (d e)
c) Zapiši novo funkcijo kvadrat, ki je definirana kot aplikacija funkcije višjega reda nekaj. Funkcija kvadrat
sprejme argument (x,y) in vrača rezultat: 𝑘𝑣𝑎𝑑𝑟𝑎𝑡(𝑥, 𝑦) = 𝑥^2 + 2𝑥 + 𝑦^2
(opomba: možnih je več
pravilnih rešitev).
fun kvadrat (x, y) = nekaj (fn a => fn b => fn c => a*a + 2*b + c*c) x (fn z => z) x y y


3. NALOGA:
Podana sta naslednja podatkovna tipa:
datatype prvi = X of int | Y of prvi | Z of drugi
 and drugi = W of prvi | tretji
datatype rezultat = P | D
Napiši vzajemno rekurzivni funkciji
preveri_prvi : prvi -> rezultat
preveri_drugi : drugi -> rezultat
ki prejmeta ustrezen izraz tipa prvi ali tipa drugi in preverita, ali se izraz konča z vrednostjo podatkovnega
tipa prvi (v tem primeru naj vrne vrednost P) ali vrednostjo podatkovnega tipa drugi (v tem primeru naj vrne
vrednost D). Primera:
- preveri_prvi (Z (W (Z (W (X 3))))); (* X 3 je tipa prvi, zato P *)
val it = P : rezultat
- preveri_prvi (Z (W (Y (Z tretji)))); (* tretji je tipa drugi, zato D *)
val it = D : rezultat
- preveri_drugi (W (Y (Z tretji))); (* tretji je tipa drugi, zato D *)
val it = D : rezultat

datatype prvi = X of int | Y of prvi | Z of drugi
 and drugi = W of prvi | tretji
datatype rezultat = P | D

fun preveri_prvi (X _) = P
  | preveri_prvi (Y p) = preveri_prvi p
  | preveri_prvi (Z d) = preveri_drugi d
and preveri_drugi tretji = D
  | preveri_drugi (W p) = preveri_prvi p


4. NALOGA:
Denimo, da podatkovne tipe prvi, drugi, rezultat in funkciji preveri_prvi ter preveri_drugi ovijemo
v modul izrazi (structure izrazi). Modulu nato pripišemo enega od podpisov (signature) izraz1,
izraz2 ali izraz3. Za vsakega izmed njih komentiraj (največ 2 povedi), kako podpis vpliva na uporabnost in
delovanje modula. Če je modul neuporaben (da napisanih funkcij ne moremo uporabljati), podaj razlog za
neuporabnost.
a.)
signature izraz1 =
sig
 type prvi
 type drugi
 datatype rezultat = D | P
 val preveri_prvi : prvi -> rezultat
 val preveri_drugi : drugi -> rezultat
end

The signature izraz1 only declares the types prvi and drugi without specifying their structure. This means that the module is unusable because we cannot construct values of these types to use with the functions preveri_prvi and preveri_drugi.

b.)
signature izraz2 =
sig
 datatype prvi = X of int | Y of prvi | Z of drugi
 datatype drugi = W of prvi | tretji
 datatype rezultat = D | P
 val preveri_prvi : prvi -> rezultat
 val preveri_drugi : drugi -> rezultat
end

The signature izraz2 fully specifies the structure of the types prvi and drugi, making the module fully usable. We can construct values of these types and use them with the functions preveri_prvi and preveri_drugi.

c.)
Zapiši novi podpis izraz3 tako, da dopolniš podpis
izraz1 (zapisane vrstice pusti nespremenjene,
lahko samo dodaš nove vrstice v podpisu) tako, da
bo deloval naslednji klic:
- izrazi.preveri_prvi (izrazi.Z (izrazi.W (izrazi.X
3)));
val it = P : izrazi.rezultat


signature izraz3 =
sig
 type prvi
 type drugi
 datatype rezultat = D | P
 val X : int -> prvi
 val Y : prvi -> prvi
 val Z : drugi -> prvi
 val W : prvi -> drugi
 val tretji : drugi
 val preveri_prvi : prvi -> rezultat
 val preveri_drugi : drugi -> rezultat
end

d.)
Kaj se zgodi, če v podpisu iz naloge c.(podpis izraz3)
spremeniš vrstico
 datatype rezultat = D | P
v
 type rezultat ?

 If you change the line datatype rezultat = D | P to type rezultat in the signature izraz3, the module becomes unusable because the type rezultat is not fully specified. We cannot construct values of type rezultat to use as the return type of the functions preveri_prvi and preveri_drugi.


1. NALOGA:
Denimo, da tokove implementiramo kot spremenljive pare (mcons) namesto kot navadne (cons).
Zapiši funkcijo (razvij tok n), ki podani tok dodatno razvije za novih n elementov. To pomeni,
da vsak klic funkcije podaljša rep toka za n elementov. Primeri:
> naravna ; tok naravnih števil
(mcons 1 #<procedure>)
> (razvij naravna 2)
> naravna
(mcons 1 (mcons 2 (mcons 3 #<procedure>))))
> (razvij naravna 2)
> naravna
(mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 #<procedure>))))))
> (razvij naravna 1)
> naravna
(mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 #<procedure>)))))))

#lang racket

(define naravna (letrec ([f (lambda (x) (mcons x (delay (f (+ x 1)))))])
                  (f 1)))

; doesnt work, it doesnt append new stream
(define (razvij tok n)
  (if (zero? n)
      tok
      (begin
        (set-mcdr! tok (force (mcdr tok)))
        (razvij (force (mcdr tok)) (- n 1)))))

