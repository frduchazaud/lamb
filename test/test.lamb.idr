#! si la première ligne du fichier commence par '#!', cette ligne est traitée comme un commentaire. Elle permet de faire d'un fichier un script exécutable par le shell.
--# directive -- pour insérer des pragmas et des annotations pour le compilateur. La directive est d'une couleur particulière pour montrer qu'elle est activée
-- # directive désactivée par l'ajout d'une espace. Elle redevient un commentaire.

--# show_packages -- affiche les packages d'où proviennent les imports. Utile ponctuellement pour le débogage

-- LE MODULE, LES EXPORTS, LE MAIN

module LambSample exposing (MyRecord, TriState, TriState.(..))

-- si on a seulement 'module LambSample' ou 'module LambSample exposing ()'
-- alors le compilateur génère une erreur : "le module LambSample n'expose aucun nom"
-- et le compilateur propose les actions suivantes :
--      1/ exporter tous les noms en général en ajoutant l'ellipse : >>> module LambSample exposing (..)
--      2/ exporter tous les noms du module explicitement :          >>> module LambSample exposing (MyRecord, TriState, TriState.(..), ...........)

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

-- Ils sont forcément placés en tête de module.
-- On ne peut pas importer entre deux fichiers.
-- On ne peut pas importer localement, sauf à l'intérieur d'une fonction.
-- Les imports ne peuvent pas se masquer entre eux : cela génère une erreur. Le compilateur propose une ou des solutions

-- Le Prelude doit être importé en premier avec une ligne qui commence par : >>> import Prelude
-- Si le module Prelude n'est pas importé, le compilateur génère une erreur.
-- Il propose alors deux actions : 
--      1/ ajouter le Prelude : >>> import Prelude exposing (..)
--      2/ spécifier que le Prelude ne doit pas être ajouté : >>> --! import Prelude
-- Cela permet :
--      soit de ne pas charger du tout Prelude pour alléger la compilation
--      soit de choisir un autre Prelude
--      soit de compléter immédiatemement après par

-- Le compilateur ajoute automatiquement un bloc de '--:' après 'import Prelude exposing ...'
-- il y ajoute les noms de du Prélude masqués par les imports. Voici :

import Prelude exposing (..) 
--: hiding
--:     ( Post      -- shadowed by @(l40c1-l40c11 >>> import Post)
--:     , List      -- shadowed by @(l55c1-l55c16 >>> import Data.List)
--:     )

-- l'IDE crée des liens navigables (clic souris) sur les balise @(l00c00-l00c00 >>> première_ligne_du_code_pointé)
-- C'est placé à cet endroit car cela caractérise le module et les imports. Les lignes et les colonnes commencent à 1.

-- si on ajoute une requête de type '--:' derrière un import le compilateur indique les noms importés de manière ordonnée comme ci-après.
-- L'IDE permet de tous les ajouter ou de tous les retirer via un menu contextuel sur tout mot-clef 'import'.

--#-------------- package base ----------------           -- ajouté automatiquement suite à la directive --# show_packages

import Prelude

-- imports are always at the beginning of the file
import Data.List 
import Data.Array as A
import Data.Maybe exposing (Maybe, maybe, withDefault, map, andThen)


--#-------------- package post ----------------

import Post
--: -- Qualified types      >>> Post.Post, Post.Mail
--: -- Qualified functions  >>> Post.estimatedReadTime, Post.encode, Post.decode  

-- si le compilateur voir un import spécifiant des noms qualifiés qui n'existent pas à l'intérieur du module importé,
-- alors il génère une erreur et propose :
--      1/ supprimer la qualification si elle correspond au module importé
--      2/ donne une liste des autres noms dont la graphie est proche avec un remplacement possible pour chacun (quand il y en a)
--      3/ supprimer le nom exposé et de proposer d'exposer le même nom s'il le trouve dans d'autres modules

-- exemple tiré de l'import ci-dessus qui pourrait être transformé facilement en joignant les deux lignes et en enlevant le '--!':
-- >>> import Post exposing (Post.Post, Post.estimatedReadTime, Post.encode, Post.decoder)

-- Le compilateur génère une erreur : 'Post.Post.estimatedReadTime' n'existe pas à l'intérieur du module 'Post'.
-- le compilateur propose alors :
--      1/ supprimer la qualification du nom exposé : remplacer 'Post.Post' par 'Post' (puisque 'Post' existe bien dans le module 'Post')
--      2/ supprimer le nom
-- et ainsi de suite pour les autres erreurs dans le 'exposing'


import Post as P
--: -- Qualified types      >>> P.Post, P.Mail
--: -- Qualified functions  >>> P.estimatedReadTime, P.encode, P.decode  

import Post exposing (Post, estimatedReadTime)
--: exposing
--:     ( Post                      -- Types
--:     , estimatedReadTime         -- Functions
--:     )
--:     -- Qualified types      >>> Post.Mail
--:     -- Qualified functions  >>> Post.encode, Post.decode  

import Post as P exposing (Post, estimatedReadTime)
--: exposing
--:     ( Post                -- Types
--:     , estimatedReadTime   -- Functions
--:     )
--:     -- Qualified types      >>> P.Mail
--:     -- Qualified functions  >>> P.encode, P.decode


--#-------------- current package ----------------

import MyLib as M



-- Haskell imports with keyword 'hiding' can be very convenient in some fast use cases
-- >>> import Data.List exposing (..) hiding (map)

-- Attention cependant à son usage. Elm l'interdit par exemple. Il est préférable de s'en passer sauf pour le Prelude.
-- L'inconvéneient est qu'on ne voit pas explicitement de quel module est importé un nom, ce qui peut créer des quiproquo.
-- L'IDE proposera des actions pour déclarer les imports qui vont automatiser tout ce travail (comme le plugin Elm dans IntelliJ)

-- Les imports sont automatiquement réorganisés par le compilateur :
-- 'import Prelude ...' est toujours le premier module importé (ou bien --! import Prelude)
-- les modules sont triés entre eux par package d'origine :
--          - en premier ceux de 'base'
--          - puis les autres packages dans l'ordre du plus grand nombre d'utilisations des noms au sein du fichier
--          - en dernier le package courant à qui appartient le module
-- le package est indiqué avec un commentaire spécial par le compilateur si la directive '--# show_packages avec --#-------------- package ... ----------------
-- Les modules importés d'un même package sont triés par nombre d'utilisation des noms (même sans 'exposing' ou 'as')
-- Les noms exposés (avec 'exposing') dans un module sont triés par nombre d'utilisations des noms.



-- IMPORT LOCAL
f k v =
    import Data.Dict as D in
    D.singleton k v
-- hors de la fonction D est inconnu.
-- il serait prudent d'interdire les "shadowing" des alias (as) de modules.
-- le "shadowing" de noms de fonctions doit rester cependant possible.

-- L'import local n'est pas possible à l'intérieur d'un 'let ... in'




-- COMMENTAIRES ET COMMENTAIRES SPECIAUX

-- Règle générale : un commentaire se place avant la ligne qu'il commente ou en fin de ligne (sauf pour les imports où '--:' est après)

-- Le commentaire de fin de ligne '--' est privilégié

{- Le commentaire de bloc ne sert qu'à désactiver un bloc de code
    il commence par '{-' et se termine par '-}'
        {- il est imbricable -}
        on peut désactiver le début du commentaire avec {--}
        on peut désactiver la fin du commentaire avec --} comme en Elm
-}

{- L'IDE propose contextuellement de :
        - transformer les commentaires de bloc en commentaires classiques
                (l'inverse n'apparaît pas pertinent à ce stade)
        - commenter un bloc sélectionné : soit en commentaire de bloc, soit en commentaires classiques
        - décommenter un bloc sélectionné
        - décommenter la région sélectionnée
-}


--# pour une directive (rappel de plus haut)

--: Annotation automatique de type par le compilateur


--:: Annotation automatique de type par le compilateur
-- la différence avec '--:' est que '--::' indique le type résultant de l'expression sur la ligne avec des noms complètement qualifiés
-- (comme si les imports n'avaient pas mentionné de 'exposing' ou de 'as'

-- le préfixe de commentaire '--:' ou '--::' est ajouté par l'utilisateur et ensuite rempli automatiquement par le compilateur.
-- Tout changement de ce commentaire spécial par l'utilisateur sera écrasé par le compilateur
-- L'IDE affiche des couleurs mettant en valeur le code, en reprenant les couleurs du code et en les estompant pour signaler qu'il n'est pas actif.
-- Typiquement, le commentaire '--:' en fin de ligne permet d'ajouter une annotation de type par le compilateur.
-- Avant une fonction le commentaire '--:' ajoute l'annotation de fonction avec le type le plus général possible
--      et sans tenir compte de l'annotation de type déjà présente (car obligatoire pour les fonctions) ... nécessité à discuter...


--! Désactivation d'une action du compilateur.

-- le '!' fait référence à ATTENTION, c'est-à-dire se rapporte aux warnings du compilateur

-- le commentaire --! permet d'annoter une ligne pour laquelle le compilateur renvoie un avertissement. Cela désactive alors l'avertissement.
-- A utiliser en connaissance de cause !
-- Utilisé pour désactiver l'import obligatoire du module Prelude avec >>> --! import Prelude
-- Utilisable pour le c

-- '' et "" et '>>>' (avec '--=' et '--:')
-- 'cette valeur est du code' : il sera mis en valeur et sera clickable comme du code normal. Dans la documentation, ce code sera aussi clickable.
-- "cette valeur est spéciale" : soit une chaîne de caractère, soit une commande ou tout autre chose qui doit être verbatim.
-- >>> ce qui suit est du code
-- si le code après '>>>' est une expression et est suivi de'--=', alors le compilateur l'exécutera et affichera le résultat. Utile pour inclure des tests
-- si le code après '>>>' n'est pas une expression et est 
-- cela sert pour illustrer ou pour les 

--# test "Nom du test facultatif" >>> du_code_à_tester_qui_doit_être_égal_à_True
-- Ces lignes de test sont placées au-dessus de la fonction testée.
-- Si le test échoue, alors le compilateur renvoie une erreur et l'indique en bout de ligne avec une croix rouge (UTF-8)
-- Si le test réussit, alors le compilateur l'indique en bout de ligne avec une coche verte (UTF-8)

---# test "Nom du test facultatif" >>> du_code_à_tester_qui_doit_être_égal_à_True
-- Idem mais sera également repris dans la documentationCela fera dans ce cas partie des propriétés ou des exemples des la fonction

-- on peut imaginer étendre cette syntaxe dans les méthodes des interfaces et générer automatiquement des tests (QuickCheck) pour les instances.
-- On ne rentre pas dans les preuves mais on pourrait finir par trouver des erreurs.
-- Comme c'est lourd à calculer, surtout à chaque fois, cela serait déclenché sur une commande : "lamb test" par exemple.



--- Documentation
-- '---' introduit une documentation pour :
--      1/ générer de la documentation avec "lamb doc"
--      2/ être reprise le compilateur pour générer ses messages d'erreur
--      3/ être reprise par l'IDE pour ses messages et son aide à la complétion
-- en effet le '--|' de Haskelle et consorts est un peu lourd à taper. On le remplace donc par '---' comme pour le '///' des langages .NET


-- To begin, here are some type declarations
-- order of declaration does not count



-- Pb de déclaration de type.
-- 3 choses différentes en Haskell.
-- Comment faire pareil en Lamb mais avec une autre syntaxe ? 

type AddressBook = List Entry
newtype AddressBook = AddressBook (List Entry) 
data AddressBook = AddressBook (List Entry) 

-- Propositions : On peut remplacer le 'newtype' par
-- NON RETENUS
type AddressBook as List Entry -- non car implique un alias dans ses autres utilisations
type AddressBook => List Entry -- non car ce n'est pas une implication. Peut porter à confusion avec les contraintes de types pour les haskellers
type AddressBook | (List Entry) -- non car syntaxe retenue pour les contraintes de type en Lamb
type AddressBook = List Entry -- non, car ne précise pas si List est un variant ou un type déjà existant.
type AddressBook ~ List Entry -- Possible car indique bien que c'est presque la même chose. La même chose mais non échangeable. Attention à la syntaxe des contraintes de type pour plus tard...
-- RETENU
newtype AddressBook = List Entry -- au moins c'est clair.
-- On peut toujours utiliser la fonctionnalité 'newtype' de Haskell avec type. Le compilateur fera le nécessaire pour éviter le boxing sur un variant unique
type Dollard = Dollard Float -- mais dans ce cas il faut déconstruire la valeur pour utiliser le type encapsulé.

addDollards : Dollard -> Dollard -> Dollard
addDollards Dollard x, Dollard y = -- on peut utiliser la virgule pour éviter de parenthèser les termes >>> addDollards (Dollard x) (Dollard y) = ...
    Dollard (x + y)

-- On pourrait aussi avoir
instance Functor Dollard
    map f (Dollard x) = Dollard (f x)
    map2 f (Dollard x) (Dollard y) = Dollard (f x y)

addDollards : Dollard -> Dollard -> Dollard
addDollards = map2 (+)

-- Un pur alias. Aucune différence avec un Int, seulement un autre nom pour être plus clair ou pour faire évoluer MyInt avec un autre type
type alias Id = String -- on pourra le remplacer par Int plus tard sans changer les signatures de fonctions. Le compilateur indiquera les modifications à effectuer sur place.

-- (Int, String, Char) is a valid type declaration itself
type alias MyTuple = (Int, String, Char)

-- you need a type constructor to create a polymorphic type in Haskell
type MyTuplePoly a = MyTuple (Int, String, a)

-- Mais ce n'est pas nécessaire en Lamb. C'est au compilateur de fournir le travail pour l'utilisateur
-- on peut aussi y ajouter des contraintes de type
type MyTuplePoly2 a b = (Int, b, a)   | (Integral a, Functor b)

-- some value declarations to illustrate type constructor
tuple1 = 
tuple1 = MyTuplePoly (1, "test", 158) --: MyTuple Int
tuple2 = MyTuplePoly 100 "test" "ccc" --: MyTuple String

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

infixr 10 :: -- sans les parenthèses
-- Est-ce la priorité et l'associativité des opérateurs est locale au module ? au package ? à l'application ?
-- la manière de Typescript peut être intéressante pour clarifier cela : >>> infixr 10 :: as Cons


--- On doit pouvoir faire du pattern matching sur les champ d'un case
testRecord : TriState -> MyRecord  -> Int
testRecord =
    \case
        State1 x, __________   -> x
        State2  , { x = 18 }   -> 1 -- peu importe le champ y du paramètre MyRecord
        ______  , { x = 3, y } -> if y >= 30 then 2 else x |> (_ * y) -- ou bien (\x -> x * y) ou bien else x * y
        ______  , r            -> r.x + (.y r)


--- A classic ADT
type Maybe a = Just a
             | Nothing

--- Un type plus complet
type Account
    = Admin String
    | Employee { name: String, corp: String, service: String, phone: PhoneNumber }
    | Client { name: String, address: Address, phone: PhoneNumber }


--- Un type récursif. L'aide de Elm est fantastique pour cela
type 

type OtherList a = List a   -- il s'agit d'un autre type, assorti de fonctions de conversions, si on en a besoin
    toList : OtherList a -> List a  -- cette fonction s'appelle : OtherList.toList. On peut l'importer comme les Variants
    toList xs = xs -- c'est le seul endroit où les conversions implicites peuvent être faites.

    fromList : List a -> OtherList a
    fromList xs = xs

--- En Haskell on aurait écrit :
newtype OtherList a = OtherList (List a)  -- Mais c'est lourd car il faut qualifier toutes les valeurs par 'OtherList'

type OtherList = List       -- KO : pas de point-free pour les créations de types. Sinon ça devient trop compliqué...

--- attention car la déclaration suivante peut poser pb
type ANewVariant a
    = List a    -- variant ou type ? Les Types et les constructeurs de type en sont pas dans les espaces de noms donc on ne peut pas savoir.
    | Vide      -- il y a un deuxième cas donc c'est un variant. Mais s'il n'y a qu'un seul cas, 

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
    \case x? of -- il s'agit d'une lambda qui envoie tous ses paramètres dans un case
        Nothing -> Nothing
        Just x  -> Just (x + 1)
    -- ok c'est exemple est vraiment artificiel. On aurait pu remplacer la Lambda par : 'Maybe.map (_ + 1)'

-- QUATRIÈME :
withDefault : Maybe a -> a -> a
withDefault =
    \case -- il s'agit d'une lambda qui envoie tous ses paramètres dans un case
        Nothing, default -> default -- on note les virgule pour séparer les cas
        Just x,  _       -> x       -- on aurait pu ajouter des annotation de type et des substitutions ('as' ou '@')

-- QUATRIÈME BIS : avec un nom utile pour la documentation de la lambda
withDefault : Maybe a -> a -> a
withDefault =
    \case x?, default of            -- facultatif : les arguments de la lambda sont nommés et séparés par des virgules. Cela documente la lambda et peut servir pour la complétion de l'IDE
        Nothing, default -> default -- l'IDE peut maintenant proposer ce cas automatiquement. Les noms dans les cas doivent correspondre exactement à ceux du \case ("shadowing" interdit)
        Just x,  ______  -> x       -- l'IDE aura d'abord proposé le cas 'Just x, default ->'. Le compilateur proposera une fois la ligne complète de remplacer 'default' par '_' car non utilisé à droite.
    -- formatage : les arguments, les '->' sont alignés verticalement. Si ce n'est pas possible, on passe au formatage de type Elm comme ci-dessous
    -- Pour faciliter le formatage, le '_' peut compter autant '_' que nécessaire.
    
-- QUATRIÈME TER : idem avec formatage sur plusieurs lignes comme en Elm.
withDefault : Maybe a -> a -> a
withDefault =
    \case x?, default of
        Nothing, default ->
            default -- avec un retrait car continuation de la ligne précédente

        Just x, _ -> -- pas d'alignement des arguments et du '=>'
            x     



--- Les LAMBDAS
-- Il y a plusieurs possibilités à évaluer
f = \x -> x + 1 -- Comme Haskell et Elm. Les parenthèses ne sont pas obligatoires
f = (\x -> x + 1) -- Idem mais avec des parenthèses. Est-ce vraiment nécessaire ? non sauf si pose un problème dans le contexte.
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
    \x, y -> x + y + 1 -- comme en Idris bien qu'ici inutile. Sert à ajouter des annotations de type ou des 'as'. Il ne s'agit pas du tuple (x,y) car le tuple est obligatoirement entouré de parenthèses
g = 
    \x: Int, y:Int -> x + y + 1  -- lla virgule permet de séparer les paramètres et d'éviter les virgules pour les annotations de type inline.


-- petit piège :
f =
    \(x, y) -> x + y + 1 -- la lambda n'a qu'un argument, le tuple '(x,y)'. On le voit grâce aux parenthèses.


-- ou bien
f =
    \x: Number, y: Number => x + y + 1 : Number
    
    -- Bien noter ici que l'espace avant le dernier':' indique qu'il concerne toute l'expression
    -- cela équivaut à 
    \x: Number, y: Number -> (x + y + 1): Number
    -- Le formatage suivant est aussi possible pour plus de clarté
    \x: Number, y: Number ->
        x + y + 1
        : Number'     -- on note l'alignement de l'indentation, qui est un peu une exception, donc à tester dans le cas limites...

    -- cette notation est très logique et doit donc être possible par cohérence au sein du langage
    -- mais en pratique elle n'est pas très utile donc à réserver aux cas où elle clarifie les choses
    -- elle pourra être supprimée si elle n'apporte rien au final, par simplicité.

    -- si le ':' avait été collé au 1, il aurait pu spécifier le type du dernier élément de l'expression (ici le '1'), ce qui n'aurait pas été clair
    -- Pour éviter ce doute pour le lecteur du code, le compilateur prend cela comme une erreur et proposera de choisir entre :
        -- 1/ Ajouter l'espace avant le ':'
        -- 2/ Mettre l'annotation à la ligne suivante
        -- 3/ Ajouter des parenthèses autour du terme typé. Ici : '\x: Number, y: Number => x + y + (1: Number)'



-- Il existe aussi un raccourci pour les lambdas : principalement utilisé avec les opérateurs
add10 = (_ + 10)
-- les parenthèses sont obligatoires, il n'y a pas de '\' en préfixe

-- Ca marche aussi avec les fonctions
flip f x y = f y x -- pour mémoire

(_ + 10) |> (map _ [1, 2, 3])
-- au lieu d'utiliser flip qui est moins clair
(_ + 10) |> flip map $ [1, 2, 3]
-- bien sûr l'exemple est tiré par les cheveux : il aurait fallu écrire plus iodimatiquement
[1, 2, 3] |$> (_ + 10)

-- TUPLES
-- Comme en Haskell, on a les opérateurs suivants : '(,)' '(,,)' '(,,,)' etc.
-- Le tuple est obligatoirement entouré de parenthèses, ça facilite le parsing et évite des équivoques comme
\x, y -> x + y -- 
1, 2, 3, 4   -- tuple sans parenthèses qui aurait pu représenter d'autres combinaisons de tuples.
(1, (2, 3), 4)
(1, (2, (3, 4)))
(((1, 2), 3), 4)
(1, 2, 3, 4)  

(,): a -> b -> (a, b)
(,) x y = (x, y) -- fonction écrite en préfixe
x, y = (x, y)    -- cette notation infixe pose des problème car les parenthèses manquent et pourtant elle serait valide.

-- Ou alors il faudrait changer le nom de l'opérateur, ce qui serait plus cohérent:
((,)): a -> b -> (a, b)
x (,) y = (x, y)
-- Mais cela signifie alors qu'un opérateur peut inclure des parenthèses, ce qui pose d'autres problèmes de parsing.

-- La bonne pratique, comme en Elm, est de ne pas utiliser les tuples au-delà de trois éléments.
-- Au-delà de trois éléments, il est recommandé d'utiliser un record, anonyme ou non.



-- LES SUBSTITUTIONS
-- on utilise le mot-clef 'as' puisqu'il est déjà disponible dans le contexte des imports.
f: Number
f =
    \case
        Nothing       , i -> Nothing
        (Just x) as x?, i -> x? |$> ( _ + x + i) -- == 2x+i
-- cela remplace le '@' de Haskell, dont la lecture n'est pas intuitive.


--- Les GUARDS

-- à la base, nous avons la cascade de 'if else'  : 'if _ then ... else if _ then ... else if _ then ... else ....'
-- c'est la construction la plus simple.
f x y = 
    if p1 then
        r1
    else if p2 then
        r2
    else
        r3

-- avec des guards à la Haskell (c'est le 'when' de Ocaml)
f x y
    | p1 = r1
    | p2 = r2
    | p3 = r3
-- franchement cette notation manque de clarté avec les '=' je trouve
-- autorisé mais générera un avertissement à la compilation avec des propositions du compilateur.

-- avec un if, c'est plus clair, notamment grâce aux flèches
f x y =
    if | p1 -> r1
       | p2 -> r2
       | p3 -> r3

-- on peut aussi en insérer dans les case
f x? y =
    let
        limit = y / 3
    in
        case x? of
            Nothing            -> "pb"
            Just x | x > limit -> "ok"
                   | otherwise -> "trop faible" -- la bonne indentation des guards est nécessaire (alignement des '|')


-- En Haskell. Pas toujours lisible. Exemple :
bmiTell :: (RealFloat a) => a -> a -> String -> String  
bmiTell weight height name  
    | bmi <= skinny = n ++ ", You're underweight, you emo, you!"  
    | bmi <= normal = n ++ ", You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = n ++ ", You're fat! Lose some weight, fatty!"  
    | otherwise     = n ++ ", You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  -- impossible de mettre un 'let ... in' dans cette construction. C'est un autre problème des guards de fonctions
          skinny = 18.5  
          (normal, fat) = (25.0, 30.0)  
          (n:_) = name  -- au passage on observe le pattern matching sur le type avec '_'. Est-ce utile ? Ne faut-il pas au contraire l'interdire ?
                        -- utile uniquement si c'est un type hole mais il faudra l'écrire autrement.



-- voici la syntaxe 'if let .... else ...' en Rust.
 // If you need to specify a failure, use an else:
    if let Some(i) = letter {
        println!("Matched {:?}!", i);
    } else {
        // Destructure failed. Change to the failure case.
        println!("Didn't match a number. Let's go with a letter!");
    }

-- Je garde en tête le '|' utilisé comme une notation en cas d'erreur dans les 'do' en Idris
f file =  do
    content <- readFile file
        | "il y a eu un pb à la lecture du fichier" -- et la fonction s'arrête net
    ...
    -- c'est à recreuser dans IDRIS car mon exemple est mauvais.



--- HOLES
-- '?Aaaa' est un "type hole", qui remplace un type (ou un nom de module ?)
-- '?aaaa' est un "typed hole", qui remplace une valeur, une fonction ou un constructeur (de valeur)
-- '?_' ou '?' est un "anonymous hole", qui remplace soit une valeur soit un type. Ce "trou" n'est pas nommé.

-- A la compilation, le compilateur remplace les "trous de type anonymes" (?_ ou ?) par le type le plus général possible (éventuellement avec contraintes)
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
        Just x -> ?aJustX



--| For later: a GADT. Avant de se lancer là-dedans, il faudrait bien réfléchir car complique fortement...
type Expr a : Num a -> Type

     ExprInt Int       : Int -> Expr
     ExprAdd Expr Expr : Expr -> Expr -> Expr
     ExprScale Int Expr: Int -> Expr -> Expr

     deriving (Eq, Ord, Enum, Bounded, Show, Read) -- default instance provided for these interfaces


--| A typeclass
interface Show a -- 'interface' en Idris, 'class' en Haskell. On peut faire sauter le 'where'. A la place on oblige à aller à la ligne et à indenter.
    show : a -> String
    -- with default implementation
    show x = "Not yet implemented"

interface Addable a    | (Integral a)   -- on ajoute des parenthèses sur la contrainte de type pour mettre en valeur le nom de l'interface
    one : a                         
    inc : a -> a
    decAbs : a -> a
    add : a -> a -> a

    -- règles de simplification pour l'optimisation
    inc .> decAbs ==> identity
    decAbs .> inc ==> identity
    -- les lois peuvent aussi être ajoutées ici sous une forme exploitable.

    -- with default implementation : dans cet exemple elles sont mutuellement récursives. L'instance devra donc fournir au moins l'une des deux.
    inc x = add x one
    add x y = 
        case x of
            0 -> y
            x -> add (decAbs x) (inc y) -- tail recursive!
            

-- Voici d'autres règles de simplification pour d'autres interface, en vrac :
guard p1 >>= guard p2 ==> guard (p1 && p2)
map f1 .> map f2 ==> map (f1 .> f2)
map f .> filter p ==> filterMap f p -- filterMap doit être implémentée dans les instances de l'interface, mais ne peut être utilisé il faut donc un mot-clef pour l'inhiber ('private', '--# no-use' ?)
fold ==> foldl -- si c'est une tableau strict, foldr sinon.



-- FONCTIONS RECURSIVES






-- CONTRAINTES DE TYPE

-- En Idris (ou Haskell avec le mot-clef 'class')
interface  (Integral a, Eq a) => Addable a

f : (Integral a) => a -> Maybe a
-- on a un problème : on aimerait le type de l'interface, ou de la fonction soit en premier

--  on peut donc imaginer que les contraintes de type soient indiquées après
interface Addable a => (Integral a, Eq a) -- plus clair à lire, sauf pour ceux qui ont l'habitude d'Haskell et de Purescript

-- Les gens qui utilisent le langage travailleront sans doute avec Haskell ou Purescript à côté.
-- Inverser la notation bien ancrée d'Haskelle va leur faire bouillir le cerveau à chaque switch de langage.

-- il vaut mieux utiliser '|', qui peut se lire "sachant que" et reste compréhensible en langage mathématique classique, en probabilité,
-- est très cohérent avec les "list comprehensions" et la syntaxe des guards et celle de mise à jour des records

-- on met quelques espaces avant le '|' au formatage (non obligatoire) et on oblige à mettre des parenthèses
interface Addable a    | (Integral a, Eq a)  
f : a -> Maybe a   | (Integral a)

-- ou bien également assez clair pour les interfaces
interface Addable a
    | (Integral a, Eq a)

    add : a -> a -> a
    add x y = x + y

    incr : a -> A
    incr x = x + 1


-- il peut parfois être nécessaire d'indiquer que certaines variables de type sont libres, avec forall
interface Addable a    | forall a. (Integral a, Eq a) 

f : a -> Maybe a   | forall a. (Integral a)
g : a -> a   | forall a  -- si rien après le forall, alors pas de point séparateur et pas de parenthèses vides.
-- le forall est facultatif quand il n'est pas nécessaire (cas très rare à ma connaissance)

reduce : (a -> a -> a) -> f a -> a   | (Foldable f, NonEmpty f, Number a) 



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

-- ou encore, mais utile hors lambdas ? Ce n'est pas idiomatique. Un avertissement proposera de le passer en ligne d'annotation si c'est possible
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
-- Ces unités sont utilisées entre chevrons, donc les majuscules comme les minuscules sont utilisables en début de token.
-- Les unités sont combinables avec ' ' pour la multiplication, ' /' pour la division et '^' pour la puissance.

-- Mass, grams.
measure g   --: measure g
-- Mass, kilograms.
measure Kg
-- Weight, pounds.
measure lb

-- Distance, meters.
measure m
-- Distance, cm
measure cm

-- Distance, inches.
measure inch
-- Distance, feet
measure ft

-- Time, seconds.
measure s

-- Force, Newtons.
measure N  = Kg m /s^2 -- mettre un point pour la multiplication ne sert à rien. Bonne pratique : '/' est préfixé d'une espace et '^' n'est entouré d'aucune.
measure N1 = m /s Kg /s -- identique. L'ordre ne compte pas
measure N2 = (m Kg) /(s)^(2) -- identique. On peut mettre des parenthèses autour des sous-expressions. Utile pour la lisibilité 
measure N3 = m Kg s^-2 -- identique. On peut mettre des puissances négatives, même sans parenthèses

measure testParen = (Kg m) / (N^2 s) -- parenthèses pour mettre sous la forme "numérateur / (termes du dénominateur)". C'est une possibilité libre.


-- Pressure, bar.
measure type bar
-- Pressure, Pascals
measure type Pa = N / m^2

-- Volume, milliliters.
measure type mL
-- Volume, liters.
measure type L

-- Define conversion constants. Introduction du mot-clef convert
convert 1_000.0<g/Kg>
convert 100.0<cm/m>
convert 2.54<cm/inch>
convert 1.0<ml/cm^3>
convert 1_000.0<ml/L>
convert 1_024<mPa/bar>

-- Pour visualiser correctement les rapports, le compilateur pourra offrir une vue en tableau ou en graphe

-- Si des rapports de conversion sont redondants, deux à deux ou en formant un cycle entre plusieurs unités, le compilateur rejette.
-- en effet, cela peut introduire des erreurs dans les calculs
convert 100.0<cm/m>     -- ok
convert 2.54<cm/inch>   -- ok
convert 5_646_516<inch/m> -- pb! Création d'un cycle

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
-- on est déjà sur de la programmation de type : on passe un type en paramètre d'une fonction !
mkMeasure :  measure a -> Float -> Float<Measure a> -- cette fonction fait appel dans son corps à la primitive convert
mkMeasure x m =
    -- x |> convert --! ne marcherait pas car convert ne peut pas changer les dimensions d'une quantité mais seulement les unités à l'intérieur d'une même dimension.
    -- x |> convert<m> --! non car ici il s'agit de mètres et non pas du paramètre de fonction m ?
    -- x<m>   --! non car 'm' est une mesure en mètre et non pas le paramètre de la fonction 'm'.
       x |> convert m -- ok. Ici le mot-clef 'convert' est utilisé comme une fonction pour forcer le type de la mesure résultante.

-- les fonctions primitives qui sont derrière le mot-clef 'convert' sont les suivantes :
-- ARITÉ 1
convert : Float<measure a> -> Float<measure b> | forall a b. (a ~ b) -- signifie que a et b sont compatibles donc peuvent décrire une même quantité.

-- ARITÉ 2
convert : measure a -> Float<measure b> -> Float<measure a>   
convert : measure a -> Float            -> Float<measure a>    -- si Float<1> /== Float



-- STYLE, IDIOMATICS

--

-- RAIL-ORIENTED PROGRAMMING AND FLOW
-- référence : https://fsharpforfunandprofit.com/rop/

-- Ce style est idiomatique et recherché. Il consiste à chaîner les fonctions.
-- Il a été institué par l'opérateur '|>' de F# et de Elm
-- Haskell utilise cet ordre-là pour les monades avec '>>=' ou même dans les do-notations.

-- le problème d'Haskell est que la lecture des fonctions se fait tantôt de droite à gauche, tantôt l'inverse
-- ce qui pose un gros problème de cohérence et de clarté du code.
-- la plupart des opérateurs haskell sont définis dans une logique de fonction mathématique utilisée de manière préfixe
-- En Haskell, la composition est '.', l'application est '$', le map est '<$>', etc.

-- En Lamb, on est sur un langage qui veut être plus lisible qu'Haskell et valoriser les transformations successives de valeurs
-- C'est d'autant plus cohérent que Lamb est non-paresseux (eager evaluation)

-- En réalité chaque opérateur peut fonctionner dans un pipe, il est logique qu'ils soient pour la plupart disponibles sous deux formes : left-to-right et right-to-left
-- 
-- On peut indiquer dans les fonctions

-- Opérateurs de Functor
-- 'map' (et non pas 'fmap'), '<$|' à la place de '<$>', et la version pipe à privilégier : '|$>'
-- Pour les opérateurs de pipe, on aurait : '|>' et l'inverse serait '$' (et non pas '<|') car le sens de '$' (application) se retrouve dans les autres opérateurs dérivés.

-- La composition serait :
-- Haskell : '.'
-- Elm et F# : '<<' et '>>'
-- Purescript : '<<<' et '>>>'
-- Flow : '.>' et '<.'     A PRIVILEGIER. Va dépendre de la disponibilité des autres opérateurs en fonctions des bibliothèques déjà existantes.

-- Refactoring automatique : avertissement du compilateur
foo : a-> b
bar : c-> d
foo $ bar x y -- À refactorer en :
foo (bar x) y


-- Nommage valeurs
-- Une valeur qui renvoie un Bool doit se commencer par "is" oue se terminer par "?"
-- Une valeur qui renvoie une alternative doit se terminer par "?". Cela concerne les Bool (sauf si commence par "is"), le Result err ok, le Maybe a... 

--- VALEURS REACTIVES
-- fonctionnalité du langage avec mots-clefs ou primitives, ou bien simple module ?

type React a = R a

x : React Int
x = R 10

x2 : React Int
x2 = [| !x * !x |] -- ou une notation comme ça, voir Idris

show x2 => "x * x"

-- Doit fonctionner avec les Effets comme Idris : Gestion du STATE
-- la gestion du graphe sous-jacent est fournie par le compilateur
-- il faudrait aussi avoir une capacité d'introspection en donnant accès au graphe mais pas facile...
-- on doit aussi pouvoir modifier les "formules" depuis l'IO sous forme de String puis les "compiler",
-- pas facile...



--| Gestion des threads
-- un thread est un Worker qui se voit attribuer des Tasks
-- Il peut exister plusieurs sortes de threads, dont certains seraient activés par défaut : UiWorker, GpuWorker, IoWorker, PureWorker



-- COMPILATION
-- inspiration notamment de Zig

noRuntimeSafetyCheck : (a -> b) -> a -> b
noRuntimeSafetyCheck f x =
    f x

tryWithRuntimeSafetyCheck : (a -> b) -> a -> b
tryWithRuntimeSafetyCheck f x =
    f x

f : Array a -> Maybe a
f? xs =
    tryWithRuntimeSafetyCheck $ xs.[180]

-- l'exemple est assez mauvais car on préfèrerait indiquer :
xs : Array a
xs.[180]  : a        -- opérateur avec runtime check -> déclenche un exception en cas de dépassement --# throw-exception IndexOutBounds
xs.[180]? : Maybe a  -- opérateur safe avec contrôle de l'index 
xs![180]  : a        -- opérateur unsafe sans contrôle de l'index -> plante de façon non contrôlée en cas de dépassement --# unsafe


-- Warning tail-recursive ! 

-- Le compilateur indique les fonctions récursives non tail-recursive avec un avertissement "not-tail-recursive". 
-- Le compilateur propose alors : 
--      1/ une ré-écriture tail-récursive si elle triviale
--      2/ désactiver le warning avec '--# not-tail-recursive'
--      3/ désactiver et ajouter un TODO avec '--TODO Need to be rewritten as tail-recursive'
-- Le compilateur ajoute un commentaire '--- tail-recursive' automatique dans la fonction d'appel (même si la vraie fonction récursive est cachée dans un 'let ... in').
-- Une fonction qui travaille sur un argument 'f a | Foldable f' et qui est tail-recursive peut être remplacée par un fold. Dans ce cas, émettre un warning

--# not-tail-recursive     -- s'il n'y a pas cette directive, cela déclenche un warning
fibbonacci : Integer -> Integer
fibbonacci =
    \case 
        n | n <= 0 -> 0
        1          -> 1
        n          -> fibbonacci (n - 1) + fibbonacci (n - 2)
        
main =
    fibbonacci 180 |> compileTime -- fibbonbacci est une fonction "pure" (ne dépendant pas d'une ressource), donc elle est appelable par compileTime
    -- contrairement à ce que l'ordre des fonctions semble indiquer, compileTime englobe bien l'appel à fibbonacci
    
    -- de manière générale, si l'optimisation est activée, le compilateur poussera tous les calculs qu'il peut effectuer à la compilation.
    -- Comme cela peut être très long, il existe la fonction noCompileTime pour l'empêcher localement.
            
main =
    do  fd <- openFile "test.txt"
        line <- getLine fd
        closeFile fd
        pure $
            case line of
                "Fibbo  please..." -> fibbonacci 180_000_000_000_000_000 |> noCompileTime
                _________________  -> 0
                
-- Lors de la super-compilation, le compilateur lance en parallèle les calculs sur les différentes branches de calcul indépendantes.
-- A l'expiration d'un time-out, il indique quelles sont les fonctions trop longues à calculer.
-- Si compileTime a été utilisé dans le code, alors il envoie un avertissement pour indiquer que le pré-calcul en cours prend trop de temps, mais sans l'interrompre.

-- noCompileTime pourra être utile pour éviter de calculer des valeurs d'initialisation pour un gigantesque tableau qui ne serait pas totalement utilisé par la suite.
-- En théorie un tel tableau devrait être un 'Coll (Lazy a)', voire être un 'Seq a'. Mais bon des contraintes liées à Lazy peuvent faire préférer de ne pas mettre de Lazy.


-- Déclenche une erreur ou un warning à la compilation
compileError : String -> never

compileWarning: String -> a -> a
compileWarning msg data =
    -- ici on génère un warning
    data
    
    -- Utilisation
    f : Int -> Int
    if heavyComputation x == pb then -- heavyComputation sera calculé à la compilation si la fonction est pure pour savoir s'il faut déclencher l'erreur. Si la fonction est impure, un warning sera généré et toutes les compileError et compileWarning à l'intérieur du if (then ou else) ou du case seront déclenchés
        compileError $ "Là on a un problème grave : " ++ pb --: never | never = Int
        else
            x |> compileWarning "Tiens ! un avertissement, faut quand même le signaler..."
            
            
-- OPTIMISATIONS
-- Une fonction qui contient un fold doit être inlinée pour permettre l'optimisation des 'fold', des 'map', des 'filter', des 'zip' successifs (à vérifier).
-- Faut-il une directive pour permettre cette optimisation ?
-- A creuser en théorie. Regarder le papier sur la méta-compilation

-- exemple
--# inline-caller
foldl : (a -> b -> a) -> a -> Coll b -> a
foldl folder acc collection = ...

f x xs =
    xs
    |> fold ...
    |> filter ...
    |> map ...
    |> map ...
    |> fold ...
    |> reduce ... -- ne short-circuit pas, donc privilégier autre chose...
            
         
    
            
            
            
            
            
-- EXCEPTIONS




-- RÈGLES D'INDENTATION

-- L'indentation est signifiante. En général, la modifier accidentellement rend le programme invalide. Quel est le niveau de risque ?
-- il n'y a pas d'alternative aux retours à la ligne comme en Haskell ';'
-- il n'y a pas d'alternative pour un bloc à l'indentation comme en Haskell '{' et '}'
-- le 'where' (qui ouvre un nouveau bloc) n'est pas nécessaire

-- Règle générale : on impose un retour à la ligne après :
--      - une expression
--      - une annotation de type
--      - une fonction
--      - un cas d'un 'case'
--      - 'module ...'
--      - 'import ...', y compris 'import ... in'
--      - 
--      - 
--      - 
-- Règles des blocs : un bloc est ouvert par
--      - 'let' d'un 'let ... in'. Il s'arrête au 'in'
--      - 'do'
--      - '\case' (sans 'of' avant '->')
--      - 'case ... of'
--      - '\case ... of'
--      - '(' jusqu'au prochain ')'
--      - '{' jusqu'au prochain '}'
--      - '[' jusqu'au prochain ']'

-- Le bloc peut commencer sur la ligne courante (après le 'do' par exemple) sauf si retour à la ligne imposé 
--      Dans ce cas, le bloc est indenté au même niveau que le début du bloc en milieu de première ligne





--
-- Coupure d'une ligne
--      On peut couper une ligne entre deux tokens.
--      La ligne se poursuit sur la ligne suivante avec une indentation supplémentaire
--      
--      SAUF si la 2e ligne commence par un opérateur infixe utilisé en position infixe (même niveau d'indentation)
--              s'il y a plusieurs coupures avant des opérateurs (ils sont donc en début de ligne),
--                  alors on pourra expérimenter pour indenter les opérateurs qui sont plus prioritaires que le précédent
--                  et désindenter si l'opérateur est moins prioritaire que le précédent, mais sans jamais désindenter au-delà du niveau d'indentation de la première ligne
--
--          ou si la première ligne se termine par ">>= \<arg-name> ->". Comme par exemple dans :
fun a =
    f a >>= \b ->
    g b >>= \c ->
    h c
--          
--          ou si la coupure se trouve à l'intérieur de parenthèses, crochets, accolades :
--              - si la ligne commence par une virgule ou le caractère fermant : la ligne n'est pas indentée
--              - sinon on indente (style préconisé : on indente de 2 caractères à cet endroit)
--
--      si coupure avant un 'in', le 'in' est indenté au même niveau que le 'let' précédent
--      si coupure avant un 

-- exemples :

f x =
    let
        tab = [ 1
              , 2
              , 3
              ]
    in
        tab |$> (_ + 1) -- map


-- NB : actuellement le ';' n'est pas utilisé dans le langage. C'est donc un séparateur disponible pour une autre construction à venir.








-- fin
