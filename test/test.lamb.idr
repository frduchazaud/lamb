#! si la première ligne du fichier commence par '#!', cette ligne est un commentaire. Elle permet de faire d'un fichier un script exécutable par le shell.
--# pour insérer des pragmas et des annotations pour le compilateur


-- LE MODULE, LES EXPORTS, LE MAIN

module LambSample exposing (MyRecord, TriState, TriState.(..) )

-- Le nom du module doit correspondre à celui du fichier (casse à l'identique)
-- Le nom du fichier est postfixé par l'extension '.lamb'
-- Le nom du module ne mentionne pas les répertoires contenant le fichier (pas de 'module Data.List' par exemple), pour une meilleure portabilité du fichier (
-- Si le nom du module et le nom du fichier diffèrent : alors avertissement avec deux actions correctives proposées :
    -- 1/ changement du nom du module
    -- 2/ changement du nom du fichier

-- Note: If you forget to add a module declaration, Elm will use this one instead:
-- >>> module Main exposing (..)

-- En Lamb : Si un seul fichier à compiler, pas besoin de déclarer le nom de module, sinon c'est une erreur.
-- Si un seul fichier sans déclaration de module alors
--      Si pas de module Main : proposera 1/ 'module Main exposing (main)' et 2/ 'module NomDuFichier exposing ()'
--      Sinon : proposition 'module NomDuFichier exposing ()'
-- Si plusieurs fichiers sans déclaration de module Main
--      Si un des fichier comporte la fonction main : proposition de 'module Main exposing (main)' et pour les autres fichiers : 'module NomDuFichier exposing ()' 
--      Sinon 'module NomDuFichier exposing ()' pour chacun.


-- Le module Main doit comporter la fonction main qui sera le point d'entrée du programme.
-- Sinon erreur : le compilateur propose de la créer.


-- Le compilateur doit comporter un outil de type cabal ou comme Elm.
-- le fichier de package (elm.json) ou cabal.yaml...) permet de construire des packages présentant une lib et ou des exécutables (plusieurs exe + test + debug...)
-- Pour construire un exécutable : il faut spécifier un module 'Main' qui expose un main
-- Pour construire une lib : les modules 'Main' sont exclus automatiquement


-- LES IMPORTS

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

-- Haskell imports with keyword 'hiding' can be very convenient in some fast use cases
-- >>> import Data.List exposing (..) hiding (map)
-- Attention cependant à son usage. Elm l'interdit par exemple. Il est préférable de s'en passer.
-- L'inconvéneient est qu'on ne voit pas explicitement de quel module est importé un nom, ce qui peut créer des quiproquo.
-- Si l'IDE est efficace, il proposera des actions pour déclarer les imports qui vont automatiser tout ce travail (comme le plugin Elm dans IntelliJ)


-- IMPORT LOCAL
f k v =
    import Data.Dict as D in
    D.singleton k v
-- hors de la fonction D est inconnu.
-- il serait prudent d'interdire les "shadowing" des alias (as) de modules.
-- le "shadowing" de noms de fonctions doit rester cependant possible.


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


-- PREMIÈRE ÉCRITURE : comme Haskell et Idris, impossible en Elm
-- Cette notation est utile pour coller aux équations mathématiques.
withDefault : Maybe a -> a -> a
withDefault Nothing  default = default
withDefault (Just x) _______ = x -- on aligne les arguments et les '='. '___' (de toute longueur) est équivalent à '_'

-- PREMIÈRE BIS : possible aussi avec les opérateurs en position infixe
(++): List Unit -> Int -> List Unit
xs ++ 0 = xs
xs ++ i = () :: (xs ++ (i - 1)) --TODO non tail recursive. Should be rewritten

-- PREMIÈRE TER : possible aussi avec les opérateurs en position préfixe, mais déconseillée. Le compilateur génére un warning et propose une réécriture en infixe.
(++): List Unit -> Int -> List Unit
(++) xs 0 = xs
(++) xs i = () :: (xs ++ (i - 1)) --TODO non tail recursive. Should be rewritten


-- DEUXIÈME plus contrainte, l'unique possible en Elm
withDefault : Maybe a -> a -> a
withDefault maybe default =
    case maybe of
        Nothing  -> default
        (Just x) -> x

-- TROISIÈME :
withDefault : Maybe a -> a -> a
withDefault =
    \case -- il s'agit d'une lambda qui envoie tous ses paramètres dans un case
        Nothing default -> default -- on note les virgule pour séparer les cas
        (Just x) _      -> x

-- TROISIÈME BIS : avec un nom utile pour la documentation de la lambda.
-- Possible avec '->' uniquement si un seul argument. Si plusieurs arguments, le compilateur fera une erreur et proposera de réécrire les cas avec '=>' au lieu de '->'
add1 : Maybe Int -> Maybe Int
add1 =
    \case x? -- il s'agit d'une lambda qui envoie tous ses paramètres dans un case
        Nothing -> Nothing
        Just x  -> Just (x + 1)
    -- ok c'est exemple est vraiment artificiel. On aurait pu remplacer la Lambda par : 'Maybe.map (_ + 1)'

-- QUATRIÈME :
withDefault : Maybe a -> a -> a
withDefault =
    \case -- il s'agit d'une lambda qui envoie tous ses paramètres dans un case
        Nothing, default => default -- on note les virgule pour séparer les cas
        Just x,  _       => x       -- on aurait pu ajouter des annotation de type et des substitutions ('as' ou '@')

-- QUATRIÈME BIS : avec un nom utile pour la documentation de la lambda
withDefault : Maybe a -> a -> a
withDefault =
    \case x?, default               -- facultatif : les arguments de la lambda sont nommés et séparés par des virgules. Cela documente la lambda et peut servir pour la complétion de l'IDE
        Nothing, default => default -- l'IDE peut maintenant proposer ce cas automatiquement. Les noms dans les cas doivent correspondre exactement à ceux du \case ("shadowing" interdit)
        Just x,  _______ => x       -- l'IDE aura d'abord proposé le cas 'Just x, default =>'. Le compilateur proposera une fois la ligne complète de remplacer 'default' par '_' car non utilisé à droite.
    -- formatage : les arguments, les '->' et les '=>' sont alignés verticalement. Si ce n'est pas possible, on passe au formatage de type Elm comme ci-dessous
    -- Pour faciliter le formatage, le '_' peut compter autant '_' que nécessaire.
    
-- QUATRIÈME TER : idem avec formatage sur plusieurs lignes comme en Elm.
withDefault : Maybe a -> a -> a
withDefault =
    \case x?, default 
        Nothing, default =>
            default -- avec un retrait car continuation de la ligne précédente

        Just x, _ => -- pas d'alignement des arguments et du '=>'
            x     



--- Les LAMBDAS
-- Il y a plusieurs possibilités à évaluer
f = \x -> x + 1 -- Comme Haskell et Elm. Les parenthèses ne sont pas obligatoires
f = (\x -> x + 1) -- Idem mais avec des parenthèses obligatoires. Est-ce vraiment nécessaire ? non sauf si pose un problème dans le contexte.
f =
    \x ->
        x + 1

-- On peut aussi imaginer les notations équivalentes suivantes :
f: Number -> Number -> Number
f =
    \x y -> x + y + 1

-- ou bien avec des annotations de type facultatives
f = 
    \(x: Number) (y: Number) -> x + y + 1 : Number

-- ou bien
f =
    \x, y => x + y + 1 -- comme en Idris bien qu'ici inutile. Sert à ajouter des annotations de type ou des 'as'. Il ne s'agit pas du tuple (x,y)

-- petit piège :
f =
    \(x, y) => x + y + 1 -- la lambda n'a qu'un argument, le tupe '(x,y)'. On le voit grâce aux parenthèses.
    
    -- pour éviter l'équivoque, le compilateur doit refuser la notation avec '=>' quand il n'y a qu'un argument et proposer '->' à la place :
    \(x, y) -> x + y + 1 -- Ouf c'est plus clair !


-- ou bien
f =
    \x: Number, y: Number => x + y + 1 : Number
    
    -- Bien noter ici que l'espace avant le dernier':' indique qu'il concerne toute l'expression
    -- cela équivaut à '\x: Number, y: Number => (x + y + 1): Number'
    -- Le formatage suivant est aussi possible pour plus de clarté
    --      '\x: Number, y: Number =>
    --                                x + y + 1
    --                                : Number'     -- on note l'alignement de l'indentation, qui est un peu une exception, donc à tester dans le cas limites...
    -- cette notation est très logique et doit donc être possible par cohérence au sein du langage
    -- mais en pratique elle n'est pas très utile donc à réserver aux cas où elle clarifie les choses
    -- elle pourra être supprimée si elle n'apporte rien au final, par simplicité.

    -- si le ':' avait été collé au 1, il aurait pu spécifier le type du dernier élément de l'expression (ici le '1'), ce qui n'aurait pas été clair
    -- Pour éviter ce doute pour le lecteur du code, le compilateur prend cela comme une erreur et proposera de choisir entre :
        -- 1/ Ajouter l'espace avant le ':'
        -- 2/ Mettre l'annotation à la ligne suivante
        -- 3/ Ajouter des parenthèses autour du terme typé. Ici : '\x: Number, y: Number => x + y + (1: Number)'



-- TUPLES
-- Comme en Haskell, on a les opérateurs suivants : '(,)' '(,,)' '(,,,)' etc.
-- Le tuple est obligatoirement entouré de parenthèses, ça facilite le parsing, évite les erreurs

(,): a -> b -> (a, b)
(,) x y = (x, y) -- fonction écrite en préfixe
x, y = (x, y)    -- cette notation infixe pose des problème car les parenthèses manquent et pourtant elle serait valide.

-- Ou alors il faudrait changer le nom de l'opérateur, ce qui serait plus cohérent:
((,)): a -> b -> (a, b)
x (,) y = (x, y)
-- Mais cela signifie alors qu'un opérateur peut inclure des parenthèses, ce qui pose d'autres problèmes de parsing.

-- La bonne pratique, comme en Elm, est de ne pas utiliser les tuples au-delà de trois éléments.
-- Au-delà de trois éléments, il est possible



-- LES SUBSTITUTIONS
-- on utilise le mot-clef 'as' puisqu'il est déjà disponible dans le contexte des imports.
f: Number
f =
    \case
        Nothing, i => (Nothing, ())
        (Just x), i -> x + i



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
-- '?Aaaa' est un "type hole", qui remplace un type (ou un nom de module ?)
-- '?aaaa' est un "typed hole", qui remplace une valeur, une fonction ou un constructeur (de valeur)
-- '?_' est un "anonymous hole", qui remplace soit une valeur soit un type. Ce "trou" n'est pas nommé.

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
    

-- FONCTIONS RECURSIVES






-- CONTRAINTES DE TYPE

-- sur les contraintes de type on peut aussi imaginer qu'elles soient indiquées après
interface Addable a => (Integral a, Eq a) -- plus clair à lire, sauf pour ceux qui ont l'habitude d'Haskell

-- ou bien encore plus clair :
interface Addable a
    => (Integral a, Eq a)

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
        fact' : Int -> Integer -> Integer
        fact' =
            \case n, acc 
                0, _ => 0
                1, acc => x
                n, acc => fact' (n - 1) (n * acc)
    in 
        fact' n 1


--| STRING USAGE
-- String is made of Char (UTF8).
-- La gestion des différentes tailles de Char implique l'intervention du compilateur
-- Byte est un entier sur 7 bits (Byte7) ou 8 bits (Byte8)
-- Regarder comment fonctionne Haskell avec Text et ByteString sur le sujet.


--| Unit Type. In Haskell the name of this type is '()'.
-- In Lamb we make a distinction btw the type and the value (unique).
-- A la compilation, ces valeurs sont effacées car elles ne servent qu'à assurer la cohérence des types.
type Unit = ()


--| expérimentation des listes strictes et des séquences paresseuses en tenant compte
-- Collection est plutôt une interface (à voir)
interface Coll a
    ...

--- Seq is a Coll with Lazy evaluation
interface Seq a 
    => (Coll a)
    ...

--- Inf is a Coll which CAN be infinite. Used to help enforce totality of computations
interface Inf a 
    take : Nat -> Inf a -> Fin a
    drop : Nat -> Inf a -> Inf a
    slice: Nat -> Nat -> Inf a 

---
interface Fin a     -- 'a' is a struct which is garanteed to be finite
    => (Inf a)      -- les opérations disponibles sur Inf sont aussi disponibles sur Fin
    count: Fin a -> Int

--| Some instances
instance Fin (List a)  -- classic linked list
instance Fin (Array a) -- boxed in the heap
instance Fin (Vect a)  -- unboxed. Collection type by defaut because the most efficient in most common cases
instance Inf (Seq a)   -- Seq peut servir à gérer des flux, des channels, des IO (sockets)

type alias FinSeq a = Fin (Seq a) 
type alias InfSeq a = Inf (Seq a) 

-- | List litterals polymorphism
-- la notation [] est polymorphique par défaut. Elle peut donc produire directement n'importe quel type de collection.

-- Mais cette notation de type [a], polymorphique sur 'a' mais également sur le type de 'Collection' pose des problèmes.
-- Voici un exemple en Haskell difficile à voir immédiatement :
hanoi_shower :: Show a => [(a, a)] -> String
hanoi_shower [(a, b)] = "Move " ++ show a ++ " to " ++ show b ++ "."

-- Le même exemple ci-dessous est plus clair au niveau de l'annotation :
hanoiPrinter : Coll (a, a) -> String     => (Show a)    -- formatage : le ':' et le '=' sont alignés (une espace avec le nom de valeur/fonction).
hanoiPrinter =                                          --             et le '=>' est distant pour plus de clarté
    \case                                               --             pas d'alignement quand il y a des arguments à la fonction, donc pas d'espace avant le ':'.
        []          -> ""
        (a, b) :: _   -> "Move " ++ show a ++ " to " ++ show b ++ "."
        
La notation polymorphique [x, y] est donc réservée aux valeurs.

-- without type annotation, it's a Vect Int // qu'il y ait un type par défaut est à discuter
oneVect = [1, 2, 3] --: Vect Int

-- Otherwise type constraint is necessary e.g. by explicit type annotation
oneList : List Char
oneList = ['a', 'b']

-- ou encore, mais utile hors lambdas ?
oneList = ['a', 'b'] : List Char

-- type constraint provided by the caller signature
testCollFn : Seq a -> Nat
testCollFn xs =
    head? xs -- head? renvoie forcément type Maybe a, et le compilateur le vérifie


-- l'annotation de fonction (son type) n'est pas fourni. Il s'agit d'une erreur que le compilateur doit corriger, en proposant un type le plus général possible.
test x? y = -- le point d'interrogation de x? contraint le type de x? à être 'Maybe a' ou 'Result err ok' ('Either l r' en Haskell). Ca marche pareil dans les 'let'
    case x? of -- ici le compilateur doit proposer une complétion par défaut pour tout type Somme
        Nothing -> 0
        Just x -> x + y -- le compilateur peut proposer une complétion pertinente avec le nom sans le point d'interrogation


-- ici on impose le type 'Seq a' à 'oneVect'. Son type par défaut ne s'applique plus et on obtient donc :
testColl = testCollFn oneVect --: Seq Int
    -- si en revanche on avait annoté oneVect :
    -- >>> oneVect : Vect Int
    -- >>> oneVect = [1, 2, 3]
    -- alors il y aurait eu un conflit de type

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
