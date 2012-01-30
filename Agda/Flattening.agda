module Flattening where

open import Data.Bool
open import Data.Product hiding (map)
open import Data.Fin hiding (_+_)
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

-- * Interpretation of types in the target language.

Segments : Set
Segments = List ℕ  -- only lengths

mutual
  PA : Type → Set
  PA int          = List ℕ
  PA bool         = List Bool
  PA (pair τ₁ τ₂) = PA τ₁ × PA τ₂
  PA (fun τ₁ τ₂)  = (F⟦ τ₁ ⟧ → F⟦ τ₂ ⟧) × (PA τ₁ → PA τ₂)
  PA (list τ)     = Segments × PA τ

  F⟦_⟧ : Type → Set
  F⟦ int ⟧        = ℕ
  F⟦ bool ⟧       = Bool
  F⟦ list τ ⟧     = PA τ
  F⟦ pair τ₁ τ₂ ⟧ = F⟦ τ₁ ⟧ × F⟦ τ₂ ⟧
  F⟦ fun τ₁ τ₂ ⟧  = (F⟦ τ₁ ⟧ → F⟦ τ₂ ⟧) × (PA τ₁ → PA τ₂)

-- NOTES: The runtime representation of functions has to contain both
-- versions, because we can, in general, not decide statically which
-- version we need. (Think of a ho-function getting a function argument
-- and using it in various ways.)
--
-- However, two versions of functions are enough. We can dynamically
-- "shift" a pair of functions up one level, i.e., we can dynamically
-- compute the doubly-lifted version from the lifted version.


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

-- Environments for the target language (flattened language).
FEnv : {n : ℕ} → Sig n → Set
FEnv = Env F⟦_⟧

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

↑ : Type → Type
↑ = list

-- * The source language

data CT : Type → Set where
  int  : CT int
  list : {τ : Type} → CT τ → CT (list τ)

data Const : Type → Set where
  num  : ℕ → Const int
  list : {τ : Type} → CT τ → List (Const τ) → Const (list τ)

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
  con  : {τ : Type} → Const τ → Expr Γ τ
  -- more base types
  cmap : {τ₁ τ₂ : Type} → Expr Γ (fun τ₁ τ₂) → Expr Γ (list τ₁) → Expr Γ (list τ₂)
  var  : (i : Fin n) → Expr Γ (lookup i Γ)

-- * Interpretation of the source language

-- Interpretation of constants in the source language.
runConst : {τ : Type} → Const τ → S⟦ τ ⟧
runConst (num n)     = n
runConst (list _ xs) = L.map runConst xs

-- Interpretation of expressions in the source language.
run : {n : ℕ} {Γ : Sig n} {τ : Type} → SEnv Γ → Expr Γ τ → S⟦ τ ⟧
run env (app e₁ e₂)  = (run env e₁) (run env e₂)
run env (lam e₁)     = λ x → run (x ∷ env) e₁
run env (add e₁ e₂)  = run env e₁ + run env e₂
run env (pair e₁ e₂) = run env e₁ , run env e₂
run env (con c)      = runConst c
run env (cmap e₁ e₂) = L.map (run env e₁) (run env e₂)
run env (var i)      = envLookup i env

-- * The target language of flattened expressions

↑Sig : {n : ℕ} → Sig n → Sig n
↑Sig = map ↑

-- I think we should not have to store an environment in clos. We are working on
-- terms, so we can inspect the entire body of the function. And the function's
-- type is parameterized by the environment. So I think we should be able to do
-- transformations on the environment of a term by traversing it in a controlled
-- fashion.
mutual
  data FExpr : {n : ℕ} → Sig n → Type → Set where
    capp   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (fun τ₁ τ₂) → FExpr Γ τ₁ → FExpr Γ τ₂
    lapp   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (↑ (fun τ₁ τ₂)) → FExpr Γ (↑ τ₁) → FExpr Γ (↑ τ₂)
    clos   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr (τ₁ ∷ Γ) τ₂ → FExpr (↑Sig (τ₁ ∷ Γ)) (↑ τ₂) → FExpr Γ (fun τ₁ τ₂)
    lclos  : {n : ℕ} {τ₁ τ₂ τ : Type} {Γ : Sig n} → FExpr (↑Sig Γ) (list τ) → FExpr (τ₁ ∷ Γ) τ₂ → FExpr (list τ ∷ ↑Sig (τ₁ ∷ Γ)) (↑ τ₂) → FExpr (↑Sig Γ) (↑ (fun τ₁ τ₂))
    add    : {n : ℕ} {Γ : Sig n} → FExpr Γ int → FExpr Γ int → FExpr Γ int
    ladd   : {n : ℕ} {Γ : Sig n} → FExpr Γ (↑ int) → FExpr Γ (↑ int) → FExpr Γ (↑ int)
    pair   : {n : ℕ} {Γ : Sig n} {τ₁ τ₂ : Type} → FExpr Γ τ₁ → FExpr Γ τ₂ → FExpr Γ (pair τ₁ τ₂)
    lpair  : {n : ℕ} {Γ : Sig n} {τ₁ τ₂ : Type} → FExpr Γ (↑ τ₁) → FExpr Γ (↑ τ₂) → FExpr Γ (↑ (pair τ₁ τ₂))
    con    : {n : ℕ} {τ : Type} {Γ : Sig n} → Const τ → FExpr Γ τ
    var    : {n : ℕ} {Γ : Sig n} → (i : Fin n) → FExpr Γ (lookup i Γ)
    dist   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ τ₁ → FExpr Γ (list τ₂) → FExpr Γ (list τ₁)
    ldist  : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (↑ τ₁) → FExpr Γ (↑ (list τ₂)) → FExpr Γ (list τ₁)
    conc   : {n : ℕ} {τ : Type} {Γ : Sig n} → FExpr Γ (list (list τ)) → FExpr Γ (list τ)
    unconc : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (list (list τ₁)) → FExpr Γ (list τ₂) → FExpr Γ (list (list τ₂))

-- * Interpretation of flattened expressions

-- In the following, we define the "class" methods corresponding
-- to class PAElem in HtM.

intlength : {τ : Type} → PA τ → ℕ
intlength {int}        a         = length a
intlength {bool}       a         = length a
intlength {list τ₁}    (d , a)   = length d
intlength {pair τ₁ τ₂} (a₁ , a₂) = intlength {τ₁} a₁
intlength {fun τ₁ τ₂}  a         = {!!} -- unclear, makes use of the environment in HtM

-- I'm not happy with this function. Not only is it not compositional,
-- it's also not quite clear to me how the representation of multiply
-- nested arrays should really look like.
interpretConst : {τ : Type} → Const τ → F⟦ τ ⟧
interpretConst (num n)                   = n
interpretConst (list int             cs) = L.map interpretConst cs
interpretConst (list (list int)      cs) = L.map (λ x → length (interpretConst x)) cs , L.concat (L.map interpretConst cs)
interpretConst (list (list (list τ)) cs) = {!!} , {!!}

intdist : {τ₁ τ₂ : Type} → F⟦ τ₁ ⟧ → PA τ₂ → PA τ₁
intdist {int} {τ₂}        e         s = L.replicate (intlength {τ₂} s) e
intdist {bool} {τ₂}       e         s = L.replicate (intlength {τ₂} s) e
intdist {list τ₁} {τ₂}    e         s = {!!}
intdist {pair τ₁ τ₂} {τ₃} (e₁ , e₂) s = intdist {τ₁} {τ₃} e₁ s , intdist {τ₂} {τ₃} e₂ s
intdist {fun τ₁ τ₂}       (e₁ , e₂) s = e₁ , e₂ -- MOST LIKELY WRONG!

interpret : {n : ℕ} {Γ : Sig n} {τ : Type} → FEnv Γ → FExpr Γ τ → F⟦ τ ⟧
interpret env (capp e₁ e₂)  = proj₁ (interpret env e₁) (interpret env e₂)
interpret env (lapp e₁ e₂)  = proj₂ (interpret env e₁) (interpret env e₂)
interpret env (add e₁ e₂)   = interpret env e₁ + interpret env e₂
interpret env (ladd e₁ e₂)  = L.zipWith _+_ (interpret env e₁) (interpret env e₂)
interpret env (con c)       = interpretConst c
interpret env (var i)       = envLookup i env
interpret env (dist {_} {τ₁} {τ₂} e₁ e₂) = intdist {τ₁} {τ₂} (interpret env e₁) (interpret env e₂)
interpret env (ldist {_} {τ₁} {τ₂} e₁ e₂) = {!intdist {list τ₁} {list τ₂}!}
interpret env (clos {n} {τ₁} {τ₂} {Γ} e₁ e₂) = (λ x → interpret (x ∷ env) e₁) , λ xs → interpret (xs ∷ envMap list (λ {τ} ys → intdist {τ} {τ₁} ys xs) env) e₂
interpret env (lclos e₀ e₁ e₂) = (λ x → interpret {!!} e₁) , λ xs → interpret (interpret env e₀ ∷ (xs ∷ env)) e₂
interpret env (conc e)      = proj₂ (interpret env e)
interpret env (unconc e₁ e₂)= proj₁ (interpret env e₁) , interpret env e₂
{-
interpret env (clos {n} {τ₁} {τ₂} {Γ} e₁ e₂)  =
  ( (λ x        → interpret (x ∷ env) e₁)
  , (λ {τ} x ys → interpret {Γ = list τ ∷ _}
                    (ys ∷ (x ∷ envMap list (λ {σ} z → interpret {Γ = σ ∷ list τ ∷ _} (z ∷ ys ∷ []) (dist (var zero) (var (suc zero)))) env)) e₂) )
interpret env (dist (lclos n e₁ e₂) e₃) = {!!} -- should produce an llclos, which we yet have to define
interpret env (dist (clos e₁ e₂) e₃) = interpret (envMap list {!!} env) (lclos e₃ e₁ e₂)
interpret env (dist e₁ e₂)  = L.replicate (length (interpret env e₂)) (interpret env e₁) -- !!!
-}
interpret env e = {!!}
{-
interpret env (clos e₁ e₂)  = λ x → interpret (x ∷ env) e₁
interpret env (lclos e₁ e₂) = {!!}
-}

-- * The flattening transformation

lemma : {n : ℕ} {i : Fin n} {Γ : Sig n} → lookup i (↑Sig Γ) ≡ ↑ (lookup i Γ)
lemma {zero} {()} {[]}
lemma {suc n} {zero}  {x ∷ xs} = refl
lemma {suc n} {suc i} {x ∷ xs} = lemma {n} {i} {xs}


mutual
  ↑Expr : {n : ℕ} {Γ : Sig n} {σ τ : Type} → Expr (σ ∷ Γ) τ → FExpr (list σ ∷ ↑Sig Γ) (↑ τ)
  ↑Expr (con c)         = dist (con c) (var zero)
  ↑Expr (app e₁ e₂)     = lapp (↑Expr e₁) (↑Expr e₂)
  ↑Expr (lam e)         = lclos (var zero) (flat e) {!!} -- lclos {!!} (flat e) (↑Expr e)
  ↑Expr (add e₁ e₂)     = ladd (↑Expr e₁) (↑Expr e₂)
  ↑Expr (pair e₁ e₂)    = lpair (↑Expr e₁) (↑Expr e₂)
  ↑Expr (cmap e₁ e₂)    = unconc (↑Expr e₂) (lapp (↑Expr e₁) (conc (↑Expr e₂))) -- TODO: most likely wrong!
     -- unconc (↑Expr e₂) (lapp (ldist (↑Expr e₁) (↑Expr e₂)) (conc (↑Expr e₂))) -- corresponds to mapP_L
     -- unconc (↑Expr e₂) (lapp (conc (ldist (dist {!flat e₁!} {!(λ {σ} z → interpret {Γ = σ ∷ list τ ∷ _} (z ∷ ys ∷ []) (dist (var zero) (var (suc zero))))!}) (↑Expr e₂))) (conc (↑Expr e₂)))
                       -- unconc (↑Expr e₂) (lapp (conc (ldist (↑Expr e₁) (↑Expr e₂))) (conc (↑Expr e₂))) -- !!!
  ↑Expr {n} {Γ} {σ} (var i) = subst (FExpr _) (lemma {suc n} {i} {σ ∷ Γ}) (var i)

  flat : {n : ℕ} {Γ : Sig n} {τ : Type} → Expr Γ τ → FExpr Γ τ
  flat (app e₁ e₂)  = capp (flat e₁) (flat e₂)
  flat (lam e)      = clos (flat e) (↑Expr e)
  flat (add e₁ e₂)  = add (flat e₁) (flat e₂)
  flat (pair e₁ e₂) = pair (flat e₁) (flat e₂)
  flat (con c)      = con c
  flat (cmap e₁ e₂) = lapp (dist (flat e₁) (flat e₂)) (flat e₂) -- corresponds to mapP_S
  flat (var i)      = var i


-- Flatten, then interpret.
frun : {n : ℕ} {Γ : Sig n} {τ : Type} → FEnv Γ → Expr Γ τ → F⟦ τ ⟧
frun env e = interpret env (flat e)

-- * Tests and examples

-- Example from Section 3.2
body₁ : Expr (int ∷ []) int
body₁ = add (add (var zero) (var zero)) (con (num 1))

↑body₁ : FExpr (list int ∷ []) (list int)
↑body₁ = ladd (ladd (var zero) (var zero)) (dist (con (num 1)) (var zero))

prop₁ : ↑Expr body₁ ≡ ↑body₁
prop₁ = refl

-- λ x → x + x + 1
example₁ : {n : ℕ} (Γ : Sig n) → Expr Γ (fun int int)
example₁ _ = lam (add (add (var zero) (var zero)) (con (num 1)))

prop₂ : flat (example₁ []) ≡ clos (add (add (var zero) (var zero)) (con (num 1))) ↑body₁
prop₂ = refl

exampleexpr : Expr [] int
exampleexpr = app (example₁ []) (con (num 3))

prop₃ : frun [] exampleexpr ≡ 7
prop₃ = refl

-- λ xs → map (λ x → x + x + 1) xs
example₂ : {n : ℕ} (Γ : Sig n) → Expr Γ (fun (list int) (list int))
example₂ _ = lam (cmap (example₁ _) (var zero))

-- (λ xs → map (λ x → x + x + 1) xs) [3,4]
exampleexpr₂ : Expr [] (list int)
exampleexpr₂ = app (example₂ _) (con (list int (num 3 ∷ (num 4 ∷ []))))

prop₄ : frun [] exampleexpr₂ ≡ 7 ∷ (9 ∷ [])
prop₄ = refl

-- map (λ y → map (λ x → x + x + 1) y)
example₂′ : Expr [] (fun (list (list int)) (list (list int)))
example₂′ = lam (cmap (example₂ _) (var zero))

exampleexpr₂′ : Expr [] (list (list int))
exampleexpr₂′ = app example₂′ (con (list (list int) []))

-- Let's play through on paper:
--
-- map (map (λ x → x + x + 1)) [[1,2],[3,4,5]]
-- map (map ⟨ λ x → x + x + 1 , λ xs → xs +L xs +L dist 1 xs ⟩) [⟨2,3⟩, [1,2,3,4,5]]
-- map (λ xs → xs +L xs +L dist 1 xs) [⟨2,3⟩, [1,2,3,4,5]]
-- (concat [⟨2,3⟩, [1,2,3,4,5]] (λ xs → xs +L xs +L dist 1 xs) (unconcat [⟨2,3⟩, [1,2,3,4,5]])
-- (concat [⟨2,3⟩, [1,2,3,4,5]] (λ xs → xs +L xs +L dist 1 xs) [1,2,3,4,5]
-- [⟨2,3⟩, [3,5,7,9,11]]

 -- λ xss → unconc xss (f_L (conc xss))   -- can't yet reproduce

-- Example from Section 6.1

example₃ : Expr [] (fun int int)
example₃ = lam (add (var zero) (con (num 1)))

-- Example from Section 6.2

example₄ : Expr [] (fun (fun int bool) (pair bool (list bool)))
example₄ = lam (pair (app (var zero) (con (num 2))) (cmap (var zero) (con (list int (num 1 ∷ (num 2 ∷ (num 3 ∷ [])))))))

-- My example:

-- map (λ x → x 1) (map (λ x y → x + y) [1,2,3])

example₅ : Expr [] (list (fun int int))
example₅ = cmap (lam (lam (add (var (suc zero)) (var zero)))) (con (list int (num 1 ∷ (num 2 ∷ (num 3 ∷ [])))))

ho_S : FExpr (fun int bool ∷ []) (pair bool (list bool))
ho_S = pair (capp (var zero) (con (num 2))) (lapp {!var zero!} {!!})