# lamb
A new functional language to test concepts


## Why
In my relentless research for a perfect world, I need a perfect functional language.
I am very inspired by:
- Haskell: no comment
- Elm: for its simplicity, its clarity, its easiness, its UI architecture (TEA)
- Idris: many improvement over Haskell. A bit to complex yet.
- herculus.io: a project for reactive functional spreadsheet with good syntax. Never see it working but interesting videos and help files

## LAMB
This name comes from:
- Lambda (calculus): of course!
- A young sheep: This language design strategy is to *follow* pre-existing languages to create a new synthesis. It is still very young and immature

## Features
- purely functional
- strict evaluation, unless specified (**Lazy** keyword)
- 
- incremental 
- efficent parser library *à la* Elm
- Elm extensible records
- With embeded UI: every project should begin with a visual interface. TEA inspired
- Reactive cells for spreadsheets with a bidirectional link (equivalence relation) between code definition and spreadsheet
- Elm type system to begin. *Typeclasses* to be added furthermore
- Idris **do** notation and **!** idioms inspiration
- Idris *Algebric Effects* inspiration to be added one day
- Elm chaining of functions - Rail oriented Programming
- Idris **Lazy** feature
- The language must be *simple* tu use, with Elm idioms and restrictive patterns of use. I have no time to put in practice too complex patterns (free monads...)
- No type dependant system
- Green threads (coroutines) available
- Agent pattern like in Erlang => Message passing coroutine collaboration
- Reflection available  in order to compile during execution 
