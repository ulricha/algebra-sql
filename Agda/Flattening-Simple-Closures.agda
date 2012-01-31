module Flattening-Simple-Closures where

open import Data.Bool
open import Data.Product hiding (map)
open import Data.Fin hiding (_+_; lift)
open import Data.Nat
open import Data.List as L hiding (map)
open import Data.Vec 
open import Function
open import Relation.Binary.PropositionalEquality

-- * Codes for types

-- Note that while we do not structurally change the types of terms
-- during flattening, their interpretation *does* change.
--
-- In particular, in the target language, function types actually
-- contain both lifted and unlifted variants. Furthermore, lists
-- actually become tables, and tables are a type-indexed type.

data Type : Set where
  int  : Type
  bool : Type
  list : Type → Type
  pair : Type → Type → Type
  fun  : Type → Type → Type
  -- α, variables?

-- * Interpretation of types in the source language.

S⟦_⟧ : Type → Set
S⟦ int ⟧        = ℕ
S⟦ bool ⟧       = Bool
S⟦ list τ ⟧     = List S⟦ τ ⟧
S⟦ pair τ₁ τ₂ ⟧ = S⟦ τ₁ ⟧ × S⟦ τ₂ ⟧
S⟦ fun τ₁ τ₂ ⟧  = S⟦ τ₁ ⟧ → S⟦ τ₂ ⟧

-- * Environments

-- Signatures describe the types of free variables. Since we're using
-- de-Bruijn-indices for free variables, we represent signatures as
-- vectors.
Sig : ℕ → Set
Sig = Vec Type

-- Environments map signatures (of types) to terms. Environments are
-- parameterized over an interpretation function on types.
data Env (⟦_⟧ : Type → Set) : {n : ℕ} → Sig n → Set where
  []  : Env ⟦_⟧ []
  _∷_ : {n : ℕ} {Γ : Sig n} {τ : Type} → ⟦ τ ⟧ → Env ⟦_⟧ Γ → Env ⟦_⟧ (τ ∷ Γ)

-- Environments for the source language.
SEnv : {n : ℕ} → Sig n → Set
SEnv = Env S⟦_⟧

-- Lookup on an environment.
envLookup : {n : ℕ} {Γ : Sig n} {⟦_⟧ : Type → Set}
            (i : Fin n) → Env ⟦_⟧ Γ → ⟦ lookup i Γ ⟧
envLookup zero    (x ∷ env) = x
envLookup (suc i) (x ∷ env) = envLookup i env

-- Mapping over an environment.
envMap : {n : ℕ} {Γ : Sig n} {⟦_⟧ : Type → Set}
         (f : Type → Type) (intf : {τ : Type} → ⟦ τ ⟧ → ⟦ f τ ⟧) → Env ⟦_⟧ Γ → Env ⟦_⟧ (map f Γ)
envMap f intf []        = []
envMap f intf (x ∷ env) = intf x ∷ envMap f intf env

-- * Interpretation of types in the target language.

Length : Set
Length = ℕ

Segments : Set
Segments = List Length

↑ : Type → Type
↑ = list

↑Sig : {n : ℕ} → Sig n → Sig n
↑Sig = map ↑

mutual
  -- PA is for "parallel array", inspired by the HtM paper. Perhaps
  -- FList (flattened list) would be a slightly more application-neutral
  -- term.
  PA : Type → Set
  PA int          = List ℕ
  PA bool         = List Bool
  PA (list τ)     = Segments × PA τ
  PA (pair τ₁ τ₂) = PA τ₁ × PA τ₂
  PA (fun τ₁ τ₂)  = Σ ℕ (λ n → Σ (Sig n) (λ Γ → Env F⟦_⟧ (↑Sig Γ) × Length × (Env F⟦_⟧ (↑Sig Γ) → PA τ₁ → PA τ₂)))

  F⟦_⟧ : Type → Set
  F⟦ int ⟧        = ℕ
  F⟦ bool ⟧       = Bool
  F⟦ list τ ⟧     = PA τ
  F⟦ pair τ₁ τ₂ ⟧ = F⟦ τ₁ ⟧ × F⟦ τ₂ ⟧
  F⟦ fun τ₁ τ₂ ⟧  = Σ ℕ (λ n → Σ (Sig n) (λ Γ → Env F⟦_⟧ Γ × (Env F⟦_⟧ Γ → F⟦ τ₁ ⟧ → F⟦ τ₂ ⟧) × (Env F⟦_⟧ (↑Sig Γ) → PA τ₁ → PA τ₂)))

-- Environments for the target language (flattened language).
FEnv : {n : ℕ} → Sig n → Set
FEnv = Env F⟦_⟧

lengthPA : (τ : Type) → PA τ → Length
lengthPA int          xs        = L.length xs
lengthPA bool         xs        = L.length xs
lengthPA (list τ)     (d  , xs) = L.length d
lengthPA (pair τ₁ τ₂) (xs , _ ) = lengthPA τ₁ xs
lengthPA (fun τ₁ τ₂)  (_ , _ , env , f , fl) = {!!} -- unclear, really -- length of first environment entry?

replicatePA : (τ : Type) → Length → F⟦ τ ⟧ → PA τ
replicatePA int          n x       = L.replicate n x
replicatePA bool         n x       = L.replicate n x
replicatePA (list τ)     n x       = L.replicate n (lengthPA τ x) , {!L.replicate n x!}
replicatePA (pair τ₁ τ₂) n (x , y) = replicatePA τ₁ n x , replicatePA τ₂ n y
replicatePA (fun τ₁ τ₂)  n (_ , _ , env , f , fl) = _ , _ , envMap ↑ (λ {τ} → replicatePA τ n) env , n , fl

-- NOTES: The runtime representation of functions has to contain both
-- versions, because we can, in general, not decide statically which
-- version we need. (Think of a ho-function getting a function argument
-- and using it in various ways.)
--
-- However, two versions of functions are enough. We can dynamically
-- "shift" a pair of functions up one level, i.e., we can dynamically
-- compute the doubly-lifted version from the lifted version.


-- * The source language

data Const : Type → Set where
  int  : Const int
  list : {τ : Type} → Const τ → Const (list τ)

C⟦_⟧ : {τ : Type} → Const τ → Set 
C⟦ int    ⟧ = ℕ
C⟦ list c ⟧ = List C⟦ c ⟧

-- I'm in favour of having less embedded applications here.
-- So pair should perhaps just be a constructor that has function
-- type. And cmap should, I think, take only one argument. That's
-- the one we're really interested in. So cmap acts like shift.
data Expr {n : ℕ} (Γ : Sig n) : Type → Set where
  app  : {τ₁ τ₂ : Type} → Expr Γ (fun τ₁ τ₂) → Expr Γ τ₁ → Expr Γ τ₂
  lam  : {τ₁ τ₂ : Type} → Expr (τ₁ ∷ Γ) τ₂ → Expr Γ (fun τ₁ τ₂)
  -- let, if?
  add  : Expr Γ int → Expr Γ int → Expr Γ int
  pair : {τ₁ τ₂ : Type} → Expr Γ τ₁ → Expr Γ τ₂ → Expr Γ (pair τ₁ τ₂)
  con  : {τ : Type} → (c : Const τ) → C⟦ c ⟧ → Expr Γ τ
  -- more base types
  cmap : {τ₁ τ₂ : Type} → Expr Γ (fun τ₁ τ₂) → Expr Γ (list τ₁) → Expr Γ (list τ₂)
  var  : (i : Fin n) → Expr Γ (lookup i Γ)

-- * Interpretation of the source language

-- Interpretation of constants in the source language.
runConst : {τ : Type} → (c : Const τ) → C⟦ c ⟧ → S⟦ τ ⟧
runConst int      n  = n
runConst (list c) xs = L.map (runConst c) xs

-- Interpretation of expressions in the source language.
run : {n : ℕ} {Γ : Sig n} {τ : Type} → SEnv Γ → Expr Γ τ → S⟦ τ ⟧
run env (app e₁ e₂)  = (run env e₁) (run env e₂)
run env (lam e₁)     = λ x → run (x ∷ env) e₁
run env (add e₁ e₂)  = run env e₁ + run env e₂
run env (pair e₁ e₂) = run env e₁ , run env e₂
run env (con c x)    = runConst c x
run env (cmap e₁ e₂) = L.map (run env e₁) (run env e₂)
run env (var i)      = envLookup i env

-- * The target language of flattened expressions

-- Interpretation of constants in the target language.
paConst : {τ : Type} → (c : Const τ) → List C⟦ c ⟧ → PA τ
paConst int      xs = xs
paConst (list c) xs = L.map length xs , paConst c (L.concat xs)

flatConst : {τ : Type} → (c : Const τ) → C⟦ c ⟧ → F⟦ τ ⟧
flatConst int             n  = n
flatConst (list int)      xs = xs
flatConst (list (list c)) xs = L.map length xs , paConst c (L.concat xs) 

lemma : {n : ℕ} (i : Fin n) (Γ : Sig n) → F⟦ lookup i (↑Sig Γ) ⟧ ≡ PA (lookup i Γ)
lemma zero    (x ∷ xs) = refl
lemma (suc i) (x ∷ xs) = lemma i xs

conc : {τ : Type} → F⟦ list (list τ) ⟧ → F⟦ list τ ⟧
conc (d , e) = e

unconc : {τ τ′ : Type} → F⟦ list (list τ′) ⟧ → F⟦ list τ ⟧ → F⟦ list (list τ) ⟧
unconc (d , _) e = (d , e)

mutual
  lift : {n : ℕ} {Γ : Sig n} {τ : Type} → FEnv (↑Sig Γ) → Length → Expr Γ τ → F⟦ ↑ τ ⟧
  lift env s (app e₁ e₂) with lift env s e₁
  ... | n , Γ , env′ , s′ , fl = fl env′ (lift env s′ e₂) -- s or s′ here?
  lift env s (lam {τ} e)   = _ , _ , env , s , λ env′ ys → lift (ys ∷ env′) (lengthPA τ ys) e
  lift env s (add e₁ e₂)   = L.zipWith _+_ (lift env s e₁) (lift env s e₂)
  lift env s (pair e₁ e₂)  = lift env s e₁ , lift env s e₂
  lift env s (con int x)   = L.replicate s x
  lift env s (con (list int) xs) = L.replicate s (length xs) , L.concat (L.replicate s xs)
  lift env s (con _ xs)    = {!!} -- this one is probably unproblematic
  lift env s (var i)       = subst id (lemma i _) (envLookup i env)
  lift env s (cmap {τ₁} {τ₂} e₁ e₂) with lift env s e₁
  ... | _ , _ , env′ , s′ , fl with lift env s′ e₂ -- s or s′ here?
  ... | xss = unconc {τ₂} {τ₁} xss (fl {!!} (conc {τ₁} xss))

  -- Flattening is an alternative interpretation.
  flat : {n : ℕ} {Γ : Sig n} {τ : Type} → FEnv Γ → Expr Γ τ → F⟦ τ ⟧
  flat env (app e₁ e₂) with flat env e₁
  ... | n , Γ , env′ , f , fl = f env′ (flat env e₂)
  flat env (lam {τ} e)  = _ , _ , env , (λ env′ x → flat (x ∷ env′) e) , λ env↑ xs → lift (xs ∷ env↑) (lengthPA τ xs) e
  flat env (add e₁ e₂)  = flat env e₁ + flat env e₂
  flat env (pair e₁ e₂) = flat env e₁ , flat env e₂
  flat env (con c x)    = flatConst c x
  flat env (cmap {τ} e₁ e₂) with flat env e₁ | flat env e₂
  ... | n , Γ , env′ , f , fl | xss = fl (envMap ↑ (λ {τ′} → replicatePA τ′ (lengthPA τ xss)) env′) xss
  flat env (var i)      = envLookup i env

-- Flatten, then interpret.
frun : {n : ℕ} {Γ : Sig n} {τ : Type} → FEnv Γ → Expr Γ τ → F⟦ τ ⟧
frun env e = flat env e

-- * Tests and examples

-- Example from Section 3.2

-- λ x → x + x + 1
example₁ : {n : ℕ} (Γ : Sig n) → Expr Γ (fun int int)
example₁ _ = lam (add (add (var zero) (var zero)) (con int 1))

exampleexpr : Expr [] int
exampleexpr = app (example₁ []) (con int 3)

prop₁ : run [] exampleexpr ≡ 7
prop₁ = refl

prop₂ : frun [] exampleexpr ≡ 7
prop₂ = refl

-- λ xs → map (λ x → x + x + 1) xs
example₂ : {n : ℕ} (Γ : Sig n) → Expr Γ (fun (list int) (list int))
example₂ _ = lam (cmap (example₁ _) (var zero))

-- (λ xs → map (λ x → x + x + 1) xs) [3,4]
exampleexpr₂ : Expr [] (list int)
exampleexpr₂ = app (example₂ _) (con (list int) (3 ∷ (4 ∷ [])))

prop₃ : run [] exampleexpr₂ ≡ 7 ∷ (9 ∷ [])
prop₃ = refl

prop₄ : frun [] exampleexpr₂ ≡ 7 ∷ (9 ∷ [])
prop₄ = refl

-- map (λ y → map (λ x → x + x + 1) y)
example₂′ : Expr [] (fun (list (list int)) (list (list int)))
example₂′ = lam (cmap (example₂ _) (var zero))

exampleexpr₂′ : Expr [] (list (list int))
exampleexpr₂′ = app example₂′ (con (list (list int)) (  (1 ∷ (2 ∷ (3 ∷ [])))
                                                     ∷ ((4 ∷ (5 ∷ []))
                                                     ∷  [])
                                                     ))

prop₅ : run [] exampleexpr₂′ ≡ (3 ∷ (5 ∷ (7 ∷ []))) ∷ ((9 ∷ (11 ∷ [])) ∷ [])
prop₅ = refl

prop₆ : frun [] exampleexpr₂′ ≡ 3 ∷ (2 ∷ []) , 3 ∷ (5 ∷ (7 ∷ (9 ∷ (11 ∷ []))))
prop₆ = refl

-- Example from Section 6.1

example₃ : Expr [] (fun int int)
example₃ = lam (add (var zero) (con int 1))

-- Example from Section 6.2

-- λ x → (x 2 , map x [1,2,3])
example₄ : Expr [] (fun (fun int int) (pair int (list int)))
example₄ = lam (pair (app (var zero) (con int 2)) (cmap (var zero) (con (list int) (1 ∷ (2 ∷ (3 ∷ []))))))

example₅ : Expr [] (pair int (list int))
example₅ = app example₄ (example₁ [])

prop₇ : run [] example₅ ≡ (5 , 3 ∷ (5 ∷ (7 ∷ [])))
prop₇ = refl

prop₈ : frun [] example₅ ≡ (5 , 3 ∷ (5 ∷ (7 ∷ [])))
prop₈ = refl

-- My examples:

-- map (λ x y → x + y) [1,2,3]
example₆ : Expr [] (list (fun int int))
example₆ = cmap (lam (lam (add (var (suc zero)) (var zero)))) (con (list int) (1 ∷ (2 ∷ (3 ∷ []))))

-- map (λ x → x 1) (map (λ x y → x + y) [1,2,3])
example₇ : Expr [] (list int)
example₇ = cmap (lam (app (var zero) (con int 1))) example₆

prop₉ : run [] example₇ ≡ 2 ∷ (3 ∷ (4 ∷ []))
prop₉ = refl

prop₁₀ : frun [] example₇ ≡ 2 ∷ (3 ∷ (4 ∷ []))
prop₁₀ = refl
  -- This needs the "length" of a PA of functions, i.e., justifies that we carry around that length in a PA of functions

-- (λ x → map (λ y → x + y) [1,2,3]) 1
example₈ : Expr [] (list int)
example₈ = app (lam (cmap (lam (add (var (suc zero)) (var zero))) (con (list int) (1 ∷ (2 ∷ (3 ∷ [])))))) (con int 1)

prop₁₁ : run [] example₈ ≡ 2 ∷ (3 ∷ (4 ∷ []))
prop₁₁ = refl

prop₁₂ : frun [] example₈ ≡ 2 ∷ (3 ∷ (4 ∷ []))
prop₁₂ = refl
  -- This needs to access the environment of the lifted function, thus requiring a lifted x.

-- map (λ x → x (x 1)) (map (λ x y → x + y) [1,2,3])
example₉ : Expr [] (list int)
example₉ = cmap (lam (app (var zero) (app (var zero) (con int 1)))) example₆

