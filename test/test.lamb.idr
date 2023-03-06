#! si la première ligne du fichier commence par '#!', cette ligne est un commentaire. Elle permet de faire d'un fichier un script exécutable par le shell.
--# pour insérer des pragmas et des annotations pour le compilateur


module LambSample exposing (MyRecord, TriState, TriState.(..) )

-- Note: If you forget to add a module declaration, Elm will use this one instead:
-- module Main exposing (..)

import Post
-- Post.Post, Post.estimatedReadTime, Post.encode, Post.decoder

import Post as P
-- P.Post, P.estimatedReadTime, P.encode, P.decoder

import Post exposing (Post, estimatedReadTime)
-- Post, estimatedReadTime
-- Post.Post, Post.estimatedReadTime, Post.encode, Post.decoder

import Post as P exposing (Post, estimatedReadTime)
-- Post, estimatedReadTime
-- P.Post, P.estimatedReadTime, P.encode, P.decoder

-- imports are always at the beginning of the file
import Data.List 
import Data.Array as A
import Data.Maybe exposing (Maybe, maybe, withDefault, map, andThen)

-- Haskell imports with keyword 'hiding' is not desirable 
-- >>> import Data.List exposing (..) hiding (map)
-- en effet si une version ultérieure du module importé ajoute des fonctions déjà présentes dans le module qui importe on aura une ambiguïté.


-- le commentaire '--:' en fin de ligne permet d'ajouter une annotation de type par le compilateur.
-- Si cette annotation a déjà été remplie, elle est vérifiée par le compilateur et mise à jour avec le type le plus général possible.
-- si on veut contraindre le type, alors il faut passer par une valeur intermédiaire dont on spécifiera le type

-- le commentaire --! permet d'annoter une ligne pour laquelle le compilateur renvoie un 



-- To begin, here are some type declarations
-- order of declaration does not count

type alias MyInt = Int

-- (Int, String, Char) is a valid type declaration iself
type alias MyTuple = (Int, String, Char)

-- you need a type constructor to create a polymorphic type
type MyTuple a = MyTuple (Int, String, a)

-- some value deeclarations to illustrate type constructor
tuple1 = MyTuple (1, "test", 158) --: MyTuple Int
tuple2 = MyTuple 100 "test" "ccc" --: MyTuple String

type alias MyRecord = 
    { x: Int
    , y: Int
    , someOtherField: MyTuple
    }

type TriState = 
    | State1 Int
    | State2
    | State3
    

type MyList a =
    | Nil
    | (::) a (MyList a) -- infix type constructors are valid

infixr 10 ::

--- '---' introduit un commentaire de documentation
--- On doit pouvoir faire du pattern matching sur les champ d'un case
testRecord : TriState -> MyRecord  -> Int
testRecord =
    \case
        State1 x, _          -> x
        State2, { x = 18 }   -> 1 -- peu importe le champ y du paramètre MyRecord
        _, { x = 3, y }      -> if y >= 30
                                    then 2
                                    else x |> (_ * y) -- ou bien (\x -> x * y) ou bien else x * y
        _, r                 -> r.x + (.y r)


--| A classic ADT  -- --| est un peu lourd à taper. On remplacera par --- comme pour le /// des langages .NET
type Maybe a = Just a
             | Nothing


-- PREMIERE ECRITURE : comme Haskell et Idris, impossible en Elm
withDefault : Maybe a -> a -> a
withDefault Nothing default = default
withDefault (Just x) _ = x

-- DEUXIEME plus contrainte, l'unique possible en Elm
withDefault : Maybe a -> a -> a
withDefault maybe default =
    case maybe of
        Nothing -> default
        (Just x) -> x

-- TROISIEME, à envisager :
withDefault : Maybe a -> a -> a
withDefault =
    \case -- il s'agit d'une lambda qui envoie tous ses paramètres dans un case
        Nothing, default -> default -- on note les virgule pour séparer les cas
        (Just x), _ -> x


--- Les LAMBDAS
-- Il y a plusieurs possibilités à évaluer
f = \x -> x + 1 -- Comme Haskell et Elm. Les parenthèses ne sont pas obligatoires
f = (\x -> x + 1) -- Idem mais avec des parenthèses obligatoires. Est-ce vraiment nécessaire ?




--- Les GUARDS

-- à la base, nous avons la cascade de 'if else'  : 'if _ then ... else if _ then ... else if _ then ... else ....'
-- c'est la construction la plus simple.
f x y = 
    if 

-- En Haskell. Pas toujours lisible je trouve.
bmiTell :: (RealFloat a) => a -> a -> String -> String  
bmiTell weight height name  
    | bmi <= skinny = n ++ ", You're underweight, you emo, you!"  
    | bmi <= normal = n ++ ", You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = n ++ ", You're fat! Lose some weight, fatty!"  
    | otherwise     = n ++ ", You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  -- impossible de mettre un 'let ... in' dans cette construction.
          skinny = 18.5  
          (normal, fat) = (25.0, 30.0)  
          (n:_) = name  -- au passage on observe le pattern matching sur le type avec '_'. Est-ce utile ? Ne faut-il pas au contraire l'interdire ?
                        -- utile uniquement si c'est un type hole.




-- voici la syntaxe 'il let .... else ...' en Rust.
 // If you need to specify a failure, use an else:
    if let Some(i) = letter {
        println!("Matched {:?}!", i);
    } else {
        // Destructure failed. Change to the failure case.
        println!("Didn't match a number. Let's go with a letter!");
    }
-- Il    




--- HOLES
-- '?Aaaa' est un "type hole", qui remplace un type
-- '?aaaa' est un "typed hole", qui remplace une valeur, une fonction ou un constructeur (de valeur)
-- '?_' remplace soit une valeur soit un type. Ce "trou" n'est pas nommé.

--
-- A la compilation, le compilateur remplace les "trous de type anonymes" (?_) par le type le plus général possible (éventuellement avec contraintes)
-- Les trous de valeur (ou de fonction ou de constructeur) sont toujours proposés au choix car il existe probablement une infinités de solutions
-- Les propositions sont triées en fonction du contexte (il est logique d'utiliser les valeurs présentes dans le contexte) et
-- aussi en fonction de leur simplicité (une seule fonction si possible, avec des arguments disponbibles dans le contexte)...

f: List ?_ -> ?Val -> List Int
f xs y =
     (_ + y) <$> ?xs -- 'map'
--donnera
f: List ?_ -> ?Val -> List Int
f xs y =
     (_ + y) <$> ?xs -- 'map'


-- autre exemple
fCase : Maybe a -> a
fCase x = 
    case ?x of ?_   
--donnera
fCase : Maybe a -> a
fCase x? = -- remplacement proposé de x en x? à cause de son type
    case x? of -- remplacement de ?x par x?
        Nothing -> ?aNothing -- les cas sont déroulés par le compilateurs et le code inconnu est complété par des trous.
        Just x -> ?x



--| For later: a GADT. Avant de se lancer là-dedans, il faudrait bien réfléchir car complique fortement...
type Expr a : Num a => Type
    ExprInt Int       : Int -> Expr
    ExprAdd Expr Expr : Expr -> Expr -> Expr
    ExprScale Int Expr: Int -> Expr -> Expr
    deriving (Eq, Ord, Enum, Bounded, Show, Read) -- default instance provided for these interfaces


--| A typeclass
interface Show a -- 'interface' en Idris, 'class' en Haskell. On peut faire sauter le 'where'. A la place on oblige à aller à la ligne et à indenter.
    show : a -> String
    -- with default implementation
    show x = "Not yet implemented"

interface (Integral a) <= Addable a -- comme en Purescript, l'opérateur est dans ce sens pour respecter le sens de l'implication. 
                                    -- on ajoute des parenthèses sur la contrainte de type pour mettre en valeur le nom de l'interface
    one : a                         
    inc : a -> a
    decAbs : a -> a
    add : a -> a -> a
    -- with default implementation : dans cet exemple elles sont mutuellement récursives. L'instance devra donc fournir au moins l'une des deux.
    inc x = add x one
    add x y = 
        case x of
            0 -> y
            x -> add (decAbs x) (inc y) -- tail recursive!
    


-- sur les contraintes de type on peut aussi imaginer qu'elles soient indiquées après
interface Addable a => (Integral a, Eq a) -- plus clair à lire, sauf pour ceux qui ont l'habitude d'Haskell
-- attention à la cohérence avec les autres endroits où on peut placer des contraintes de types.

--| Instances declarations are terse in Idris (no keyword)
--| But Haskell ones are easiers to read (and to parse!). Donc on prend celle d'Haskell
instance Show MyRecord
    show { x, y, someOtherField, _ } = "{ x=" ++ show x ++ ", y" ++ show y  ++ ", somesomeOtherField=" ++ show someOtherField ++ " }"

-- l'exemple de ces instances est inutile, car on voudrait pouvoir dériver automatiquement certaines interfaces comme 'Show', avec 'deriving'
instance Show MyTuple
    show (i, s, c) = "(" ++ show i ++ ", " ++ show s ++ ", " show c ++ ")"

--| Some functions
fact : Nat -> Integer
fact n =
    let
        fact' : Integer -> Integer
        fact' 0 = 0
        fact' 1 = 1
        fact' n = n *        
    in 
        fact' (n |> toInteger) -- style : fact est récursive, donc l'appel est en premier

--| String usage
-- String is made of Char (UTF8).
-- La gestion des différentes tailles de Char implique l'intervention du compilateur
-- Byte est un entier sur 7 bits (Byte7) ou 8 bits (Byte8)
-- Regarder comment fonctionne Haskell avec Text et ByteString sur le sujet.


--| Unit Type. In Haskell the name of this type is '()'.
-- In Lamb we make a distinction btw the type and the real value (unique).
type Unit = ()

--| expérimentation des listes strictes et des séquences paresseuses en tenant compte
-- Collection est plutôt une interface (à voir)
interface Coll a = [a] -- '=' notation here reserved to primitive or a new kind of alias ?
interface alias Coll a = [a] --  alternative clearer

interface Seq a -- Coll with Lazy evaluation

interface Inf a where -- struct which CAN be infinite. Used to help enforce totality of computations
    take : Nat -> Inf a -> Fin a
    drop : Nat -> Inf a -> Inf a
    slice: Nat -> Nat -> Inf a 

interface Fin a -- 'a' is a struct which is garanteed to be finite


-- les opérations disponibles sur Inf sont aussi disponibles sur Fin
instance Inf (Fin a)

--| Some instances
instance Coll (List a) -- classic linked list
instance Coll (Array a) -- boxed in the heap
instance Coll (Vect a) -- unboxed. Collection type by defaut because the most efficient in most common cases
instance Coll (Seq a) -- Seq peut servir à gérer des flux, des channels, des IO (sockets)

type alias FinSeq a = Fin (Seq a) 
type alias InfSeq a = Inf (Seq a) 

-- | List litterals polymorphism
-- la notation [] est polymorphique par défaut. Elle peut donc produire directement n'importe quel type de collection.

-- Mais cette notation de type [a], polymorphique sur 'a' mais également sur le type de 'Collection' pose des problèmes.
-- Voici un exemple en Haskell difficile à voir immédiatement :
hanoi_shower :: Show a => [(a, a)] -> String
hanoi_shower [(a, b)] = "Move " ++ show a ++ " to " ++ show b ++ "."

-- Le même exemple ci-dessous est plus clair au niveau de l'annotation :
hanoiPrinter : Show a => Coll (a, a) -> String
hanoiPrinter =
    \case
        []          -> ""
        (a, b)::_   -> "Move " ++ show a ++ " to " ++ show b ++ "."
        
La notation polymorphique [x, y] est donc réservée aux valeurs.

-- without type annotation, it's a Vect Int // qu'il y ait un type par défaut est à discuter
-- oneVect : Vect Int
oneVect = [1, 2, 3]

-- Otherwise type constraint is necessary e.g. by explicit type annotation
oneList : List Char
oneList = ['a', 'b']

-- type constraint provided by the caller signature
testCollFn : Seq a -> Nat
testCollFn xs =
    head? xs -- head? renvoie forcément type Maybe a, et le compilateur le vérifie


-- l'annotation de fonction (son type) n'est pas fourni. Il s'agit d'une erreur que le compilateur doit corriger, en proposant un type le plus général possible.
test x? y = -- le point d'interrogation de x? contraint le type de x? à être 'Maybe a' ou 'Result err ok' ('Either l r' en Haskell). Ca marche pareil dans les 'let'
    case x? of -- ici le compilateur doit proposer une complétion par défaut pour tout type Somme
        Nothing -> 0
        Just x -> x + y -- le compilateur peut proposer une complétion pertinente avec le nom sans le point d'interrogation


-- ici on impose le type 'Seq a' à 'oneVect'. Son type par défaut ne s'applique plus
-- et on obtient donc en réalité => oneVect : Seq Int
testColl = testCollFn oneVect

-- Convention : quand une fonction renvoie un 'Maybe a', son nom doit se terminer par '?'
-- Idem pour un 'Result err ok'
-- à ne pas confondre avec la notation ?typedHole
head? : Seq a -> Maybe a
head =
    \case
        []           -> Nothing -- coll literal : ici lu comme un 'Seq'
        x :: _ -> Just x  -- le constructeur :: appartient soit à List soit à Seq. Le compilateur utilise le bon en fonction du contexte, donc 'Seq'

-- le fonctionnement des Maybe





    
    




-- | List comprehensions (LC)
-- une LC a pour type [a] // ie Collection a
-- si le type de la valeur générée n'est pas spécifié
-- (par annotation de type ou par inférrence de type)
-- alors la notation est un
lc1 = []


-- | Evaluation strategy
-- strictly evaluated
-- special type Lazy, comme dans Idris : le compilateur fait le boulot :
--  * pas la peine d'envoyer un type Lazy dans la fonction (avec 'defer')
--  * pas la peine de lancer explicitement le calcul du thunk dans la fonction (avec 'force')
--
-- L'évaluation stricte reste prioritaire quand le compilateur ne peut déterminer s'il doit calculer les thunks.

-- Exemple Lazy simple
ifFn : Bool -> Lazy a -> Lazy a -> a
ifFn True  thn _   = thn
ifFn False _   els = els


-- Exemple complet
-- BEGIN EXAMPLE
calculSuperLong : Int -> Int
calculSuperLong x = ...

testLazy : Lazy Int -> Lazy Char -> Lazy String -> Bool -- on peut imaginer qu'en interne le compilateur peut inliner 2^3 versions de la fonction pour gérer les Lazy.
testLazy li lc ls =                                     -- certaines de ces versions seront inutiles et élaguées, ou alors il encapsule au besoin les paramètres Lazy dans des thunk
    if lc = 'a' then -- déclenche le calcul de 'lc' si c'est un thunk
        True
    else if ls = "stop" then -- déclenche le calcul de 'ls' (si c'est un thunk)
        True
    else
        li == 0 -- déclenche le calcul de manière conditionnelle, si la valeur est ensuite utilisée sur un site d'appel non Lazy.
                -- permet d'économiser des calculs potentiellement longs, à condition que ce gain soit déterminé de manière certaine par le compilateur dans le contexte
                -- Donc : choix déterminé statiquement par le compilateur. Si doute, alors calcul lancé.
                -- Dans l'absolu, le compilateur pourrait générer deux versions de cette fonction : une qui renvoie un "Bool" et une qui renvoie un "Lazy Bool"
                -- Si le programmeur veut absolument du Lazy en retour, il lui suffit de l'indiquer dans le type de retour de la fonction

testLazyConsummer : Bool -> Result Bool String
testLazyConsummer launchCalculus =
    let 
        testLazyVal =
            testLazy (calculSuperLong 2) 'c' "ok" -- on ne sait pas encore s'il faut le calculer. Le calcul sera déclenché si besoin
    in
    if launchCalculus then
        Ok <| testLazyVal -- le résultat de testLazy est consommé à coup sûr. On lance le calcul.
    else
        Err "pb!" -- le réstultat de testLazy n'est pas consommé. Inutile de le calculer

--

testLazyReturnsLazy : Lazy Int -> Lazy Bool
testLazyReturnsLazy li =
    li == 0 -- ne lance pas le calcul mais retourne un thunk

testLazyConsummer2 : Bool
testLazyConsummer2 =
    testLazyReturnsLazy 1 -- lance le calcul du thunk retourné.

-- END EXAMPLE


--| Fonctionnalité d'agent ou d'acteur (Erlang / Gleam...)
-- Voir notamment : https://hackage.haskell.org/package/hactors-0.0.3.1/docs/Control-Concurrent-Actor.html
-- Utile pour concevoir les types qui vont derrière
-- Cependant, dans un souci de simplicité, il serait mieux de l'intégrer au langage comme le fait Erlang

-- voici une syntaxe inspirée d'Erlang. Le 'receive' fonctionne comme un 'case' ou un 'select' en go
actor: Actor Pid -> Pid
actor pid autrePid
receive
     Message1 -> send autrePid "voici un message"
     Message2 -> expr2
after
     Timeout ->
        ActionTimeOut

register pid -- what?
spawn (actor pid)

-- Tentative de typage...
type alias ActorRef msg = 
  { refId   :: ThreadId
  , refMbox :: TQueue msg
  } deriving (Eq)

type alias Behaviour msg = { getBehaviour :: msg -> IO (Behaviour msg) }

spawn : Behaviour msg -> IO (ActorRef msg)
send : ActorRef msg -> msg -> IO ()



--| Voici les types de Control.Concurrent.Actor (Haskell)
type alias Process = ThreadId

self : IO Process
kill : IO Process -> IO ()
exit : IO ()
sleep : Int -> IO ()
wait : IO ()
say : String -> IO ()

type MBox m = TChan m
type alias Actor m =
    { proc :: Process
    , mbox :: MBox m
    }

actor : t -> (t -> MBox m -> IO a) -> IO (Actor m)
spawn : (MBox m -> IO a) -> IO (Actor m)
receive :: MBox m -> (m -> IO a) -> IO b
(?) : MBox m -> (m -> IO a) -> IO b
(<?) : IO (MBox m) -> (m -> IO a) -> IO b
send : Actor m -> m -> IO m
(!) : Actor m -> m -> IO m
(<!) : IO (Actor m) -> m -> IO m
(!>) : Actor m -> IO m -> IO m
(<!>) : IO (Actor m) -> IO m -> IO m
spawn_receive: (m -> IO a) -> IO (Actor m)
on_exception : IO a -> IO a -> IO a
tolerant : IO a -> IO a
faultable : IO () -> IO ()

-- FIN des types de Control.Concurrent.Actor (Haskell)


--| Unités : inspiration du F#
-- Les unités ne sont utilisables que sur les Float, Double car basés sur des ratio et des multiplications/divisions nécessaires pour les conversions.
-- Les nombres rationnels (Integral/Integral) ne sont pas possibles car ne permettent pas de gérer les unités avec des puissances décimales, comme la racine carrée : <m^0.5>
-- Ces unités sont utilisées entre chevrons, donc les majuscules comme les minuscules sont utilisables.

-- Mass, grams.
measure type g
-- Mass, kilograms.
measure type Kg
-- Weight, pounds.
measure type lb

-- Distance, meters.
measure type m
-- Distance, cm
measure type cm

-- Distance, inches.
measure type inch
-- Distance, feet
measure type ft

-- Time, seconds.
measure type s

-- Force, Newtons.
measure type N = Kg m / s^2
measure type N = m /s Kg /s -- identique

-- Pressure, bar.
measure type bar
-- Pressure, Pascals
measure type Pa = N / m^2

-- Volume, milliliters.
measure type mL
-- Volume, liters.
measure type L

-- Define conversion constants. Introduction du mot-clef convert
convert 1000.0<g/Kg>
convert 100.0<cm/m>
convert 2.54<cm/inch>
convert 1.0<ml/cm^3>
convert 1000.0<ml/L>
convert 1024<mPa/bar>

-- Pour visualiser correctement les rapports, le compilateur pourra offrir une vue en tableau ou en graphe

-- Si des rapports de conversion sont redondants, deux à deux ou en formant un cycle entre plusieurs unités, le compilateur rejette.
-- en effet, cela peut introduire des erreurs dans les calculs
convert 100.0<cm/m>     -- ok
convert 2.54<cm/inch>   -- ok
convert 5646516<inch/m) -- pb! Création d'un cycle

-- avec cette notation déclarative, les rapports de conversion sont constants et ne peuvent être redéfinis au cours de l'exécution d'un programme.
-- les monnaies ne peuvent donc être modélisés par ce biais.
-- de plus les rapports de conversion entre monnaies comportent des cycles et pis ! Ces cycles peuvent ne pas être cohérents ponctuellement.
-- pour modéliser les monnaies il vaut mieux utiliser des type phantômes.

-- Le compilateur doit fournir également des outils de conversion automatique d'unités
-- soit basé sur les annotations de type
volumeSphère : Float<m> -> Float<cm^3>
volumeSphère radius =
    radius * radius * radius --: Float<m^3>
        |> convert -- la fonction 'convert' est une primitive (mot-clef) qui effectue les conversions entre types compatibles

-- conversion qui peut servir aussi à la normalisation (forme canonique)
g : Float<N/g>
g = 1.901<m/s^2> |> convert

-- Une unité de mesure est une classe de types bien particulière
-- On doit pouvoir construire une valeur avec une unité
-- cette fonction n'est pas nécessaire dans Prelude.Measures
mkMeasure : Float -> Measure -> Float<Measure> -- cette fonction fait appel dans son corps à des primitives
mkMeasure x m =
    -- x |> convert --! ne marcherait pas car convert ne peut pas changer les dimensions d'une quantité mais seulement les unités à l'intérieur d'une même dimension.
    -- x |> convert<m> --! non car ici il s'agit de mètres et non pas du paramètre de fonction m ?
    -- x |> convert m -- ok. Ici le mot-clef 'convert' est utilisé comme une fonction



--- VALEURS REACTIVES
-- fonctionnalité du langage avec mots-clefs ou primitives, ou bien simple module ?

x : React Int
x = R 10

x2 : React Int
x2 = [| !x * !x |] -- ou une notation comme ça, voir Idris

-- Doit fonctionner avec les Effets comme Idris : Gestion du STATE
-- la gestion du graphe sous-jacent est fournie par le compilateur
-- il faudrait aussi avoir une capacité d'introspection en donnant accès au graphe mais pas facile
-- on doit aussi pouvoir modifier les "formules" depuis l'IO sous forme de String puis les "compiler",
-- pas facile...



--| Gestion des threads
-- un thread est un Worker qui se voit attribuer des Tasks
-- Il peut exister plusieurs sortes de threads, dont certains seraient activés par défaut : UiWorker, GpuWorker, IoWorker, PureWorker


-- fin
