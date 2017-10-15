module Aviary.Birds (
      applicator
    , bluebird
    , bluebird'
    , blackbird, (...)
    , bunting, (<.<.<)
    , becard 
    , cardinal
    , cardinal', (>..)
    , cardinalstar
    , cardinalstarstar, (>..<)
    , dove, (<..<)
    , dickcissel, (<...<)
    , dovekie
    , eagle
    , baldeagle
    , finch
    , finchstar
    , goldfinch
    , hummingbird
    , idiot
    , idstar
    , idstarstar
    , psi
    , jay 
    , jalt
    , jalt'
    , kestrel
    , kite
    , phoenix
    , queer
    , quixotic
    , quizzical
    , quirky
    , quacky
    , owl, (.$.)
    , robin 
    , robinstar
    , robinstarstar
    , starling
    , starling'
    , thrush
    , vireo
    , vireostarstar
    , warbler
    , worbler
    , warblerstar
    , warblerstarstar
    ) where 

import Prelude ( class Semigroupoid
               , class Category
               , (<<<), (>>>)
               , ($) 
               , id
               , ap
               , const
               , flip )

import Control.Monad ( class Bind
                     , class Monad
                     , (=<<), (>>=)
                     , join )

import Control.Apply ( class Apply 
                     , lift2 )


-- | A combinator - applicator
-- | 
-- | (a → b) → a → b
-- | `λ f x . f x`
applicator ∷ ∀ a b. (a → b) → a → b
applicator = ($)


-- | B combinator - bluebird 
-- | S(KS)K
-- | (b → c) → (a → b) → a → c
-- | `λ g f x . g (f x)`
bluebird ∷ ∀ a b c d. Semigroupoid a ⇒ a c d → a b c → a b d
bluebird = (<<<)


-- | B' combinator - bluebird prime
-- | BB
-- | (a → c → d) → a → (b → c) → b → d
-- | `λ f x g y . f x (g y)`
bluebird' ∷ ∀ a b c d e. Semigroupoid b ⇒ (a → b d c) → a → b e d → b e c 
bluebird' = (<<<)(<<<)


-- | B1 combinator - blackbird
-- | BBB
-- | (c → d) → (a → b → c) → a → b → d
-- | `λ f g x y . f (g x y)`
blackbird ∷ ∀ a b c d e. Semigroupoid e ⇒ e b a → (d → e c b) → d → e c a
blackbird = (<<<)<<<(<<<)
infixr 9 blackbird as ...


-- | B2 combinator - bunting 
-- | B(BBB)B
-- | (d → e) → (a → b → c → d) → a → b → c → e 
-- | `λ f g x y z . f (g x y z)`
bunting ∷ ∀ a b c d e f. Semigroupoid b ⇒ b d c → (f → a → b e d) → f → a → b e c
bunting = (<<<)<<<(<<<)<<<(<<<)
infixr 9 bunting as <.<.<


-- | B3 combinator - becard
-- | B(BB)B
-- | (c → d) → (b → c) → (a → b) → a → d
-- | `λ f g h x . f (g (h x))`
-- TODO
becard ∷ ∀ a b c d. (c → d) → (b → c) → (a → b) → a → d
becard f g h x = f $ g $ h x -- (<<<(<<<)) <<< (...)


-- | C combinator - cardinal
-- | S(BBS)(KK)
-- | (a → b → c) → b → a → c
-- | `λ f x y . f y x`
cardinal ∷ ∀ a b c. (a → b → c) → b → a → c  
cardinal = flip


-- | C' combinator - cardinal prime
-- | S(BBS)(KK)
-- | (c → a → d) → (b → c) → a → b → d
-- | `λ f g x y . f (g y) x`
cardinal' ∷ ∀ a b c d e. Semigroupoid b ⇒ b d c → (b e c → a) → b e d → a
cardinal' = (>>>)<<<(<<<)
infixr 9 cardinal' as >..


-- | C* combinator - cardinal once removed
-- | BC
-- | (a → c → b → d) → a → b → c → d
-- | `λ f x y z . f x z y`
cardinalstar ∷ ∀ a b c d. Semigroupoid a ⇒ a d c → a c b → a d b
cardinalstar = (>>>)


-- | C** combinator - cardinal twice removed
-- | BC*
-- | (a → b → d → c → e) → a → b → c → d → e
-- | `λ f s t u v . f s t v u`
cardinalstarstar ∷ ∀ a b c d e. Semigroupoid b ⇒ ((b e d → b e c) → a) → b d c → a -- TODO
cardinalstarstar = (>>>)(<<<)
infixr 9 cardinalstarstar as >..<


-- | D combinator - dove
-- | BB
-- | (a → c → d) → a → (b → c) → b → d
-- | `λ f x g y . f x (g y)`
dove ∷ ∀ a b c d e. Semigroupoid b ⇒ (a → b d c) → a → b e d → b e c
dove = (<<<)(<<<) -- ~ bluebird'
infixr 9 dove as <..<


-- | D1 combinator - dickcissel
-- | B(BB)
-- | (a → b → d → e) → a → b → (c → d) → c → e
-- | `λ f x y g z . f x y (g z)`
dickcissel ∷ ∀ a b c d e. Semigroupoid b ⇒ b d c → (a → b e d) → a → b e c
dickcissel = (<<<)(<<<)(<<<)
infixr 9 dickcissel as <...<


-- | D2 combinator - dovekie
-- | BB(BB)
-- | (c → d → e) → (a → c) → a → (b → d) → b → e
-- | `λ f g x h z . f (g x) (h z)`
dovekie ∷ ∀ a b c d e f. Semigroupoid c ⇒ c e d → (a → b → c f e) → a → b → c f d
dovekie = (<..<)(...) 


-- | E combinator - eagle
-- | B(BBB) 
-- | (a → d → e) → a → (b → c → d) → b → c → e
-- | `λ f x g y z . f x (g y z)`
eagle ∷ ∀ a b c d e f. Semigroupoid c ⇒ (a → b → c e d) → a → b → c f e → c f d
eagle = (...)(<<<)


-- | Ê combinator - bald eagle
-- | B(BBB)(B(BBB))
-- | (e → f → g) → (a → b → e) → a → b → (c → d → f) → c → d → g
-- | `λ f g s t h u v . f (g s t) (h u v)`
baldeagle ∷ ∀ a b c d e f g. Semigroupoid d ⇒ d g c → (f → d b g) → f → (e → d a b) → e → d a c
baldeagle = (...)(<...<)<<<(...)


-- | F combinator - finch
-- | ETTET
-- | a → b → (b → a → c) → c
-- | `λ x y f . f y x`
finch ∷ ∀ a b c. a → (c → a → b) → c → b
finch = flip (flip >>> id)


-- | F* combinator - finch once removed 
-- | BC*R*
-- | (c → b → a → d) → a → b → c → d
-- | `λ f x y z . f z y x`
finchstar ∷ ∀ a b c e d f. Semigroupoid e ⇒ (f → (e b a → e c a) → d) → e c b → f → d
finchstar = (>>>)(>>>) <<< flip


-- | G combinator - goldfinch
-- | BBC
-- | (b → c → d) → (a → c) → a → b → d
-- | `λ f g x y . f y (g x)`
goldfinch ∷ ∀ a b c d. (b → c → d) → (a → c) → a → b → d 
goldfinch = (<..<) flip


-- | H combinator - hummingbird
-- | BW(BC)
-- | (a → b → a → c) → a → b → c 
-- | `λ f x y . f x y x`
-- TODO
hummingbird ∷ ∀ a b m. Bind m ⇒ m a → (a → m b) → m b
hummingbird = (>>=)


-- | I combinator - identity bird 
-- | SKK
-- | a → a 
-- | `λ x . x`
idiot ∷ ∀ t a. Category a ⇒ a t t  
idiot = id 


-- | I* combinator - id bird once removed
-- | S(SK)
-- | (a → b) → a → b
-- | `λ f x . f x`
idstar ∷ ∀ a b. (a → b) → a → b 
idstar = id 


-- | I** combinator - id bird twice removed 
-- | 
-- | (a → b → c) → a → b → c
-- | `λ f x y . f x y`
idstarstar ∷ ∀ a b c. (a → b → c) → a → b → c 
idstarstar = id 


-- | Psi combinator - psi bird - `on`
psi ∷ ∀ a b c. (b → b → c) → (a → b) → a → a → c
psi f g = \x y → g x `f` g y


-- | J combinator - Jay
-- | B(BC)(W(BC(B(BBB))))
-- | (a → b → b) → a → b → a → b
-- | `λ f x y z . f x (f z y)`
jay ∷ ∀ a b. (a → b → b) → a → b → a → b
jay = lift2 flip ((<<<)(...)) flip


-- | Alternative J combinator - Joy
-- | 
-- | (a → c) → a → b → c
-- | `λf x y . f x`
jalt ∷ ∀ a b c d e. Semigroupoid b ⇒ a → b d c → b e d → b e c
jalt = const (<<<)


-- | J' combinator - Joy prime
-- | 
-- | (a → b → d) → a → b → c → d
-- | `λ f x y z . f x y`
jalt' ∷ ∀ a b c d e f. Semigroupoid e ⇒ a → (f → e c d) → f → e b c → e b d
jalt' = const (<..<)


-- | K combinator - kestrel
-- | K
-- | a → b → a
-- | `λ x y . x`
kestrel ∷ ∀ a b. a → b → a 
kestrel = const 


-- | Ki combinator - kite (false)
-- | KI
-- | a → b → b
-- |`λ x y . y`
kite ∷ ∀ a b c. Category b ⇒ a → b c c
kite = const id 


-- | Φcombinator - phoenix 
-- | 
-- | (b → c → d) → (a → b) → (a → c) → a → d
-- | `λ f g h x . f (g x) (h x)`
phoenix ∷ ∀ a b c f. Apply f ⇒ (a → b → c) → f a → f b → f c
phoenix = lift2


-- | Q combinator - queer bird
-- | CB
-- | (a → b) → (b → c) → a → c
-- | `λ f g x . g (f x)`
queer ∷ ∀ a b c d. Semigroupoid a ⇒ a b c → a c d → a b d
queer = (>>>)


-- | Q1 combinator - quixotic bird
-- | BCB
-- | (b → c) → a → (a → b) → c
-- | `λ f x g . f (g x)`
quixotic ∷ ∀ a b c. (b → c) → a → (a → b) → c
quixotic = (>>>)(flip id) <<< (<<<)


-- | Q2 combinator - quizzical bird 
-- | C(BCB)
-- | a → (b → c) → (a → b) → c
-- | `λ x f g . f (g x)`
quizzical ∷ ∀ a b c. a → (b → c) → (a → b) → c
quizzical = (>>>) <<< flip id


-- | Q3 combinator - quircky bird
-- | BT
-- | (a → b) → a → (b → c) → c
-- | `λ f x g . g (f x)`
quirky ∷ ∀ a b c. (a → b) → a → (b → c) → c
quirky = (<<<)(flip id)


-- | Q4 combinator - quacky bird
-- | F*B
-- | a → (a → b) → (b → c) → c
-- | `λ x f g . g (f x)`
quacky ∷ ∀ a b c. c → (c → a) → (a → b) → b
quacky = (<<<)(flip id) <<< flip id


-- | O combinator - owl
-- | SI 
-- | ((a → b) → a) → (a → b) → b
-- | `λ x y . y (x y)`
owl ∷ ∀ a b c d e. Semigroupoid c ⇒ (b → c e d) → b → c a e → c a d
owl = (<<<)(<<<) -- ap id -- bluebird' 
infixr 8 owl as .$.


-- | R combinator - robin
-- | BBT
-- | a → (b → a → c) → b → c
-- | `λ x f y . f y x`
robin ∷ ∀ a b c. a → (b → a → c) → b → c
robin = flip flip 


-- | R* combinator - robin once removed 
-- | C*C*
-- | (b → c → a → d) → a → b → c → d
-- | `λ f x y z . f y z x`
-- TODO
--robinstar ∷ ∀ a b c. (b → a) → b → (a → c) → c
--robinstar = flip <<< (>>>)
robinstar ∷ ∀ a b c d. (b → c → a → d) → a → b → c → d
robinstar f x y z = f y z x 


-- | R** combinator - robin twice removed 
-- | BR*
-- | (a → c → d → b → e) → a → b → c → d → e
-- | `λ f s t u v . f s u v t`
-- TODO
--robinstarstar = (<<<)(flip<<<(>>>))
robinstarstar ∷ ∀ a b c d e. (a → c → d → b → e) → a → b → c → d → e
robinstarstar f s t u v = f s u v t


-- | S combinator - starling
-- | S
-- | (a → b → c) → (a → b) → a → c
-- | `λ f g x . f x (g x)`
starling ∷ ∀ m a b. Monad m ⇒ m (a → b) → m a → m b
starling = ap


-- | S' combinator - starling prime
-- | 
-- | (b → c → d) → (a → b) → (a → c) → a → d
-- | `λ f g h x . f (g x) (h x)`
starling' ∷ ∀ a b c f. Apply f ⇒ (a → b → c) → f a → f b → f c
starling' = lift2 


-- | T combinator - thrush
-- | CI
-- | a → (a → b) → b
-- | `λ x f . f x`
thrush ∷ ∀ a b. a → (a → b) → b
thrush = flip id 


-- | V combinator - vireo
-- | BCT
-- | a → b → (a → b → c) → c
-- | `λ x y f . f x y`
vireo ∷ ∀ a b c. c → b → (c → b → a) → a
vireo = flip <<< flip id 


-- | V* combinator - vireo once removed
-- | C*F*
-- | (b → a → b → d) → a → b → b → d
-- | `λ f x y z . f y x z`
vireostar ∷ ∀ a b c. (a → b → c) → b → a → c
vireostar = flip


-- | V** combinator - vireo twice removed
-- | BV*
-- | (a → c → b → c → e) → a → b → c → c → e
-- | `λ f s t u v . f s v t u`
-- | TODO
vireostarstar ∷ ∀ a b c d.  (a → c → b → c → d) → a → b → c → c → d
vireostarstar = (<<<)(((<<<)flip)<<<flip)


-- | W combinator - warbler - omega 
-- | MM
-- | (a → a → b) → a → b
-- | `λ f x . f x x`
warbler ∷ ∀ a m. Bind m ⇒ m (m a) → m a
warbler = join 


-- | W1 combinator - converse warbler 
-- | CW
-- | a → (a → a → b) → b
-- | `λ x f = f x x`
worbler ∷ ∀ a b. b → (b → b → a) → a
worbler = flip =<< flip id 


-- | W* combinator - warbler once removed 
-- | BW
-- | (a → b → b → c) → a → b → c
-- | `λ f x y . f x y y`
-- TODO
warblerstar ∷ ∀ a b c. (a → b → b → c) → a → b → c
warblerstar f x y = f x y y -- flip flip id <<< (<<<) ap


-- | W** combinator - warbler twice removed 
-- | BV*
-- | (a → b → c → c → d) → a → b → c → d
-- | `λ f x y z . f x y z z`
-- TODO
warblerstarstar ∷ ∀ a b c d. (a → b → c → c → d) → a → b → c → d 
warblerstarstar f x y z = f x y z z 
