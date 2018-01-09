module Aviary.Birds (module Aviary.Birds) where 

import Prelude ( class Semigroupoid
               , class Category
               , (<<<), (>>>)
               , ($) 
               , id
               , ap
               , const
               , compose
               , flip )

import Control.Monad ( class Bind
                     , class Monad
                     , (=<<), (>>=)
                     , join )

import Control.Apply ( class Apply 
                     , lift2 )


infixl 1 t      as &
infixr 8 cd     as .:
infixr 8 cdc    as .:.
infixr 8 cdd    as .::
infixr 8 cddc   as .::.
infixr 8 cddd   as .:::
infixr 8 cdddc  as .:::.
infixr 8 cdddd  as .::::
infixr 8 cddddc as .::::.


-- | Fixed point Y combinator 
-- |
-- | `Λ a . (a → a) → a`
-- |
-- | `λ f . (λ x. f (x x)) (λ x . f (x x))`
fix ∷ ∀ a . (a → a) → a
fix f = compose f fix $ f 


-- | Reverse application which is 
-- | probably exist inside `Lens` module
t ∷ ∀ a b . a → (a → b) → b
t = flip ($)


-- | A combinator - applicator
-- | 
-- | `Λ a b . (a → b) → a → b`
-- |
-- | `λ f x . f x`
applicator ∷ ∀ a b . (a → b) → a → b
applicator = ($)


-- | B combinator - bluebird 
-- |
-- | S(KS)K
-- |
-- | `Λ a b c . (b → c) → (a → b) → a → c`
-- |
-- | `λ g f x . g (f x)`
bluebird ∷ ∀ a b c d . Semigroupoid a ⇒ a c d → a b c → a b d
bluebird = compose


-- | B' combinator - bluebird prime
-- |
-- | BB
-- |
-- | `Λ a b c d . (a → c → d) → a → (b → c) → b → d`
-- |
-- | `λ f x g y . f x (g y)`
bluebird' ∷ ∀ a b c d e . Semigroupoid b ⇒ (a → b d c) → a → b e d → b e c 
bluebird' = compose compose


-- | B1 combinator - blackbird
-- |
-- | BBB
-- |
-- | `Λ a b c d . (c → d) → (a → b → c) → a → b → d`
-- |
-- | `λ f g x y . f (g x y)`
blackbird ∷ ∀ a b c d e . Semigroupoid e ⇒ e b a → (d → e c b) → d → e c a
blackbird = (<<<)<<<(<<<)
infixr 9 blackbird as ...


-- | B2 combinator - bunting 
-- |
-- | B(BBB)B
-- |
-- | `Λ a b c d e . (d → e) → (a → b → c → d) → a → b → c → e`
-- |
-- | `λ f g x y z . f (g x y z)`
bunting ∷ ∀ a b c d e f . Semigroupoid b ⇒ b d c → (f → a → b e d) → f → a → b e c
bunting = (<<<)<<<(<<<)<<<(<<<)
infixr 9 bunting as <.<.<


-- | B3 combinator - becard
-- |
-- | B(BB)B
-- |
-- | `Λ a b c d . (c → d) → (b → c) → (a → b) → a → d`
-- |
-- | `λ f g h x . f (g (h x))`
-- TODO
becard ∷ ∀ a b c d . (c → d) → (b → c) → (a → b) → a → d
becard = flip (<<<)(<<<) <<< (...)


-- | C combinator - cardinal
-- |
-- | S(BBS)(KK)
-- |
-- | `Λ a b c . (a → b → c) → b → a → c`
-- |
-- | `λ f x y . f y x`
cardinal ∷ ∀ a b c . (a → b → c) → b → a → c  
cardinal = flip


-- | C' combinator - cardinal prime
-- |
-- | S(BBS)(KK)
-- |
-- | `Λ a b c d (c → a → d) → (b → c) → a → b → d`
-- |
-- | `λ f g x y . f (g y) x`
cardinal' ∷ ∀ a b c d e . Semigroupoid b ⇒ b d c → (b e c → a) → b e d → a
cardinal' = (>>>)<<<(<<<)
infixr 9 cardinal' as >..


-- | C* combinator - cardinal once removed
-- |
-- | BC
-- |
-- | `Λ a b c d . (a → c → b → d) → a → b → c → d`
-- |
-- | `λ f x y z . f x z y`
cardinalstar ∷ ∀ a b c d . Semigroupoid a ⇒ a d c → a c b → a d b
cardinalstar = (>>>)


-- | C** combinator - cardinal twice removed
-- |
-- | BC*
-- |
-- | `Λ a b c d e . (a → b → d → c → e) → a → b → c → d → e`
-- |
-- | `λ f s t u v . f s t v u`
cardinalstarstar ∷ ∀ a b c d e . Semigroupoid b ⇒ ((b e d → b e c) → a) → b d c → a
cardinalstarstar = (>>>)(<<<)
infixr 9 cardinalstarstar as >..<


-- | D combinator - dove
-- |
-- | BB
-- |
-- | `Λ a b c d . (a → c → d) → a → (b → c) → b → d`
-- |
-- | `λ f x g y . f x (g y)`
dove ∷ ∀ a b c d e . Semigroupoid b ⇒ (a → b d c) → a → b e d → b e c
dove = compose compose
infixr 9 dove as <..<


-- | D1 combinator - dickcissel
-- |
-- | B(BB)
-- |
-- | `Λ a b c d e . (a → b → d → e) → a → b → (c → d) → c → e`
-- |
-- | `λ f x y g z . f x y (g z)`
dickcissel ∷ ∀ a b c d e . Semigroupoid b ⇒ b d c → (a → b e d) → a → b e c
dickcissel = (<<<)(<<<)(<<<)
infixr 9 dickcissel as <...<


-- | D2 combinator - dovekie
-- |
-- | BB(BB)
-- |
-- | `Λ a b c d e . (c → d → e) → (a → c) → a → (b → d) → b → e`
-- |
-- | `λ f g x h z . f (g x) (h z)`
dovekie ∷ ∀ a b c d e f . Semigroupoid c ⇒ c e d → (a → b → c f e) → a → b → c f d
dovekie = (<..<)(...) 


-- | E combinator - eagle
-- |
-- | B(BBB) 
-- |
-- | `Λ a b c d e . (a → d → e) → a → (b → c → d) → b → c → e`
-- |
-- | `λ f x g y z . f x (g y z)`
eagle ∷ ∀ a b c d e f . Semigroupoid c ⇒ (a → b → c e d) → a → b → c f e → c f d
eagle = (...)(<<<)


-- | Ê combinator - bald eagle
-- |
-- | B(BBB)(B(BBB))
-- |
-- | `Λ a b c d e f g . (e → f → g) → (a → b → e) → a → b → (c → d → f) → c → d → g`
-- |
-- | `λ f g s t h u v . f (g s t) (h u v)`
baldeagle ∷ ∀ a b c d e f g . Semigroupoid d ⇒ d g c → (f → d b g) → f → (e → d a b) → e → d a c
baldeagle = (...)(<...<)<<<(...)


-- | F combinator - finch
-- |
-- | ETTET
-- |
-- | `Λ a b c . a → b → (b → a → c) → c`
-- |
-- | `λ x y f . f y x`
finch ∷ ∀ a b c . a → (c → a → b) → c → b
finch = flip (flip >>> id)


-- | F* combinator - finch once removed 
-- |
-- | BC*R*
-- |
-- | `Λ a b c d . (c → b → a → d) → a → b → c → d`
-- |
-- | `λ f x y z . f z y x`
finchstar ∷ ∀ a b c e d f . Semigroupoid e ⇒ (f → (e b a → e c a) → d) → e c b → f → d
finchstar = (>>>)(>>>) <<< flip


-- | G combinator - goldfinch
-- |
-- | BBC
-- |
-- | `Λ a b c d . (b → c → d) → (a → c) → a → b → d`
-- |
-- | `λ f g x y . f y (g x)`
goldfinch ∷ ∀ a b c d . (b → c → d) → (a → c) → a → b → d 
goldfinch = (<..<) flip


-- | H combinator - hummingbird
-- |
-- | BW(BC)
-- |
-- | `Λ a b c (a → b → a → c) → a → b → c`
-- |
-- | `λ f x y . f x y x`
hummingbird ∷ ∀ a b m . Bind m ⇒ m a → (a → m b) → m b
hummingbird = (>>=)


-- | I combinator - identity bird 
-- |
-- | SKK
-- |
-- | `Λ a . a → a` 
-- |
-- | `λ x . x`
idiot ∷ ∀ t a . Category a ⇒ a t t  
idiot = id 


-- | I* combinator - id bird once removed
-- |
-- | S(SK)
-- |
-- | `Λ a b . (a → b) → a → b`
-- |
-- | `λ f x . f x`
idstar ∷ ∀ a b . (a → b) → a → b 
idstar = id 


-- | I** combinator - id bird twice removed 
-- | 
-- | `Λ a b c . (a → b → c) → a → b → c`
-- |
-- | `λ f x y . f x y`
idstarstar ∷ ∀ a b c . (a → b → c) → a → b → c 
idstarstar = id 


-- | Psi combinator - psi bird - `on`
-- | 
-- | `Λ a b . (b → b → c) → (a → b) → a → a → c`
-- |
-- | `λ f g . λ x y . f (g x) (g y) `
on ∷ ∀ a b c . (b → b → c) → (a → b) → a → a → c
on f g = \a b → g a `f` g b


-- | J combinator - Jay
-- |
-- | B(BC)(W(BC(B(BBB))))
-- |
-- | `Λ a b c d . (a → b → b) → a → b → a → b`
-- |
-- | `λ f x y z . f x (f z y)`
jay ∷ ∀ a b . (a → b → b) → a → b → a → b
jay = lift2 flip ((<<<)(...)) flip


-- | Alternative J combinator - Joy
-- | 
-- | `Λ a b c . (a → c) → a → b → c`
-- |
-- | `λf x y . f x`
jalt ∷ ∀ a b c d e . Semigroupoid b ⇒ a → b d c → b e d → b e c
jalt = const (<<<)


-- | J' combinator - Joy prime
-- | 
-- | `Λ a b c d . (a → b → d) → a → b → c → d`
-- |
-- | `λ f x y z . f x y`
jalt' ∷ ∀ a b c d e f . Semigroupoid e ⇒ a → (f → e c d) → f → e b c → e b d
jalt' = const (<..<)


-- | K combinator - kestrel
-- |
-- | K
-- |
-- | `Λ a b . a → b → a`
-- |
-- | `λ x y . x`
kestrel ∷ ∀ a b . a → b → a 
kestrel = const 


-- | Ki combinator - kite (false)
-- |
-- | KI
-- |
-- | `Λ a b . a → b → b`
-- |
-- | `λ x y . y`
kite ∷ ∀ a b c . Category b ⇒ a → b c c
kite = const id 


-- | Φ combinator - phoenix 
-- | 
-- | `Λ a b c d . (b → c → d) → (a → b) → (a → c) → a → d`
-- |
-- | `λ f g h x . f (g x) (h x)`
phoenix ∷ ∀ a b c f . Apply f ⇒ (a → b → c) → f a → f b → f c
phoenix = lift2


-- | Q combinator - queer bird
-- |
-- | CB
-- |
-- | `Λ a b c . (a → b) → (b → c) → a → c`
-- |
-- | `λ f g x . g (f x)`
queer ∷ ∀ a b c d . Semigroupoid a ⇒ a b c → a c d → a b d
queer = (>>>)


-- | Q1 combinator - quixotic bird
-- |
-- | BCB
-- |
-- | `Λ a b c . (b → c) → a → (a → b) → c`
-- |
-- | `λ f x g . f (g x)`
quixotic ∷ ∀ a b c . (b → c) → a → (a → b) → c
quixotic = (>>>)(flip id) <<< (<<<)


-- | Q2 combinator - quizzical bird 
-- |
-- | C(BCB)
-- |
-- | `Λ a b c . a → (b → c) → (a → b) → c`
-- |
-- | `λ x f g . f (g x)`
quizzical ∷ ∀ a b c . a → (b → c) → (a → b) → c
quizzical = (>>>) <<< flip id


-- | Q3 combinator - quircky bird
-- |
-- | BT
-- |
-- | `Λ a b c . (a → b) → a → (b → c) → c`
-- |
-- | `λ f x g . g (f x)`
quirky ∷ ∀ a b c . (a → b) → a → (b → c) → c
quirky = (<<<)(flip id)


-- | Q4 combinator - quacky bird
-- |
-- | F*B
-- |
-- | `Λ a b c . a → (a → b) → (b → c) → c`
-- |
-- | `λ x f g . g (f x)`
quacky ∷ ∀ a b c . c → (c → a) → (a → b) → b
quacky = (<<<)(flip id) <<< flip id


-- | O combinator - owl
-- |
-- | SI 
-- |
-- | `Λ a b . ((a → b) → a) → (a → b) → b`
-- |
-- | `λ x y . y (x y)`
owl ∷ ∀ a b c d e . Semigroupoid c ⇒ (b → c e d) → b → c a e → c a d
owl = compose compose 
infixr 8 owl as .$.


-- | R combinator - robin
-- |
-- | BBT
-- |
-- | `Λ a b c . a → (b → a → c) → b → c`
-- |
-- | `λ x f y . f y x`
robin ∷ ∀ a b c . a → (b → a → c) → b → c
robin = flip flip 


-- | R* combinator - robin once removed 
-- |
-- | C*C*
-- |
-- | `Λ a b c d . (b → c → a → d) → a → b → c → d`
-- |
-- | `λ f x y z . f y z x`
robinstar ∷ ∀ a b c d . (b → c → a → d) → a → b → c → d
robinstar = flip <<< (<<<) flip


-- | R** combinator - robin twice removed 
-- |
-- | BR*
-- |
-- | `Λ a b c d e . (a → c → d → b → e) → a → b → c → d → e`
-- |
-- | `λ f s t u v . f s u v t`
robinstarstar ∷ ∀ a b c d e . (a → c → d → b → e) → a → b → c → d → e
robinstarstar = (<<<) $ flip <<< (<<<) flip


-- | S combinator - starling
-- |
-- | S
-- |
-- | `Λ a b c . (a → b → c) → (a → b) → a → c`
-- |
-- | `λ f g x . f x (g x)`
starling ∷ ∀ m a b . Monad m ⇒ m (a → b) → m a → m b
starling = ap


-- | S' combinator - starling prime
-- | 
-- | `Λ a b c d . (b → c → d) → (a → b) → (a → c) → a → d`
-- |
-- | `λ f g h x . f (g x) (h x)`
starling' ∷ ∀ a b c f . Apply f ⇒ (a → b → c) → f a → f b → f c
starling' = lift2 


-- | T combinator - thrush
-- |
-- | CI
-- |
-- | `Λ a b . a → (a → b) → b`
-- |
-- | `λ x f . f x`
thrush ∷ ∀ a b . a → (a → b) → b
thrush = flip id 


-- | V combinator - vireo
-- |
-- | BCT
-- |
-- | `Λ a b c . a → b → (a → b → c) → c`
-- |
-- | `λ x y f . f x y`
vireo ∷ ∀ a b c . c → b → (c → b → a) → a
vireo = flip <<< flip id 


-- | V* combinator - vireo once removed
-- |
-- | C*F*
-- |
-- | `Λ a b c d . (b → a → b → d) → a → b → b → d`
-- |
-- | `λ f x y z . f y x z`
vireostar ∷ ∀ a b c . (a → b → c) → b → a → c
vireostar = flip


-- | V** combinator - vireo twice removed
-- |
-- | BV*
-- |
-- | `Λ a b c d e . (a → c → b → c → e) → a → b → c → c → e`
-- |
-- | `λ f s t u v . f s v t u`
vireostarstar ∷ ∀ a b c d . (a → c → b → c → d) → a → b → c → c → d
vireostarstar = (<<<) $ ((<<<) flip) <<< flip


-- | W combinator - warbler - omega 
-- |
-- | MM
-- |
-- | `Λ a b . (a → a → b) → a → b`
-- |
-- | `λ f x . f x x`
warbler ∷ ∀ a m . Bind m ⇒ m (m a) → m a
warbler = join 


-- | W1 combinator - converse warbler 
-- |
-- | CW
-- |
-- | `Λ a b . a → (a → a → b) → b`
-- |
-- | `λ x f = f x x`
worbler ∷ ∀ a b . b → (b → b → a) → a
worbler = flip =<< flip id 


-- | W* combinator - warbler once removed 
-- |
-- | BW
-- |
-- | `Λ a b c . (a → b → b → c) → a → b → c`
-- |
-- | `λ f x y . f x y y`
warblerstar ∷ ∀ a b c . (a → b → b → c) → a → b → c
warblerstar = flip flip id <<< (<<<) ap


-- | W** combinator - warbler twice removed 
-- |
-- | BV*
-- |
-- | `Λ a b c d . (a → b → c → c → d) → a → b → c → d`
-- |
-- | `λ f x y z . f x y z z`
warblerstarstar ∷ ∀ a b c d . (a → b → c → c → d) → a → b → c → d 
warblerstarstar = flip flip id <<< (<<<) (flip <<< (<<<) ap)


-- | compose2
cd ∷ ∀ p a b c d . Semigroupoid p
   ⇒ p a b
   → (d → p c a)
   → (d → p c b)
cd = dove compose


-- | compose3
cdc ∷ ∀ p a b c d e . Semigroupoid p
    ⇒ p a b
    → (c → d → p e a)
    → (c → d → p e b)
cdc = dove cd


-- | compose4
cdd ∷ ∀ p a b c d e f . Semigroupoid p
    ⇒ p e f
    → (a → b → c → p d e)
    → (a → b → c → p d f)
cdd = dove cdc


-- | compose5
cddc ∷ ∀ p a b c d e f g . Semigroupoid p
     ⇒ p a b
     → (c → d → e → f → p g a)
     → (c → d → e → f → p g b)
cddc = dove cdd


-- | compose6
cddd ∷ ∀ p a b c d e f g h . Semigroupoid p
     ⇒ p a b
     → (c → d → e → f → g → p h a)
     → (c → d → e → f → g → p h b)
cddd = dove cddc


-- | compose7
cdddc ∷ ∀ p a b c d e f g h i . Semigroupoid p
      ⇒ p a b
      → (c → d → e → f → g → h → p i a)
      → (c → d → e → f → g → h → p i b)
cdddc = dove cddd


-- | compose8
cdddd ∷ ∀ p a b c d e f g h i j . Semigroupoid p
      ⇒ p a b
      → (c → d → e → f → g → h → i → p j a)
      → (c → d → e → f → g → h → i → p j b)
cdddd = dove cdddc


-- | compose9
cddddc ∷ ∀ p a b c d e f g h i j k . Semigroupoid p
       ⇒ p a b
       → (c → d → e → f → g → h → i → j → p k a)
       → (c → d → e → f → g → h → i → j → p k b)
cddddc = dove cdddd 
