module Flattening where

open import Data.Bool
open import Data.Product hiding (map)
open import Data.Fin hiding (_+_)
open import Data.Nat
open import Data.List as L hiding (map)
open import Data.Vec 
open import Function
open import Relation.Binary.PropositionalEquality
 
data Type : Set where
  int  : Type
  bool : Type
  list : Type → Type
  pair : Type → Type → Type
  fun  : Type → Type → Type
  -- α, variables?

⟦_⟧ : Type → Set
⟦ int ⟧        = ℕ
⟦ bool ⟧       = Bool
⟦ list τ ⟧     = List ⟦ τ ⟧
⟦ pair τ₁ τ₂ ⟧ = ⟦ τ₁ ⟧ × ⟦ τ₂ ⟧
⟦ fun τ₁ τ₂ ⟧  = ⟦ τ₁ ⟧ → ⟦ τ₂ ⟧

F⟦_⟧ : Type → Set
F⟦ int ⟧        = ℕ
F⟦ bool ⟧       = Bool
F⟦ list τ ⟧     = List F⟦ τ ⟧
F⟦ pair τ₁ τ₂ ⟧ = F⟦ τ₁ ⟧ × F⟦ τ₂ ⟧
F⟦ fun τ₁ τ₂ ⟧  = (F⟦ τ₁ ⟧ → F⟦ τ₂ ⟧) × ({τ : Type} → F⟦ list τ₁ ⟧ → F⟦ list τ ⟧ → F⟦ list τ₂ ⟧)

Sig : ℕ → Set
Sig = Vec Type

data Env : {n : ℕ} → Sig n → Set where
  []  : Env []
  _∷_ : {n : ℕ} {Γ : Sig n} {τ : Type} → F⟦ τ ⟧ → Env Γ → Env (τ ∷ Γ)

envLookup : {n : ℕ} {Γ : Sig n} (i : Fin n) → Env Γ → F⟦ lookup i Γ ⟧
envLookup zero    (x ∷ env) = x
envLookup (suc i) (x ∷ env) = envLookup i env

envMap : {n : ℕ} {Γ : Sig n} (f : Type → Type) (intf : {τ : Type} → F⟦ τ ⟧ → F⟦ f τ ⟧) → Env Γ → Env (map f Γ)
envMap f intf []        = []
envMap f intf (x ∷ env) = intf x ∷ envMap f intf env

data Const : Type → Set where
  num  : ℕ → Const int
  list : {τ : Type} → List (Const τ) → Const (list τ)

data Expr {n : ℕ} (Γ : Sig n) : Type → Set where
  app  : {τ₁ τ₂ : Type} → Expr Γ (fun τ₁ τ₂) → Expr Γ τ₁ → Expr Γ τ₂
  lam  : {τ₁ τ₂ : Type} → Expr (τ₁ ∷ Γ) τ₂ → Expr Γ (fun τ₁ τ₂)
  -- let, if?
  add  : Expr Γ int → Expr Γ int → Expr Γ int
  con  : {τ : Type} → Const τ → Expr Γ τ
  -- more base types
  cmap : {τ₁ τ₂ : Type} → Expr Γ (fun τ₁ τ₂) → Expr Γ (list τ₁) → Expr Γ (list τ₂)
  var  : (i : Fin n) → Expr Γ (lookup i Γ)

↑ : Type → Type
↑ = list
{-
↑ int          = list int
↑ bool         = list bool
↑ (list τ)     = list (list τ)
↑ (pair τ₁ τ₂) = pair (↑ τ₁) (↑ τ₂)
↑ (fun τ₁ τ₂)  = list (fun τ₁ τ₂)
-}

↑Sig : {n : ℕ} → Sig n → Sig n
↑Sig = map ↑

mutual
  data FExpr : {n : ℕ} → Sig n → Type → Set where
    capp   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (fun τ₁ τ₂) → FExpr Γ τ₁ → FExpr Γ τ₂
    lapp   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (↑ (fun τ₁ τ₂)) → FExpr Γ (↑ τ₁) → FExpr Γ (↑ τ₂)
    clos   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr (τ₁ ∷ Γ) τ₂ → ({τ : Type} → FExpr (list τ ∷ ↑Sig (τ₁ ∷ Γ)) (↑ τ₂)) → FExpr Γ (fun τ₁ τ₂)
    lclos  : {n : ℕ} {τ₁ τ₂ τ : Type} {Γ : Sig n} → FExpr (τ₁ ∷ Γ) τ₂ → FExpr (list τ ∷ ↑Sig (τ₁ ∷ Γ)) (↑ τ₂) → FExpr (list τ ∷ ↑Sig Γ) (↑ (fun τ₁ τ₂))
    add    : {n : ℕ} {Γ : Sig n} → FExpr Γ int → FExpr Γ int → FExpr Γ int
    ladd   : {n : ℕ} {Γ : Sig n} → FExpr Γ (↑ int) → FExpr Γ (↑ int) → FExpr Γ (↑ int)
    con    : {n : ℕ} {τ : Type} {Γ : Sig n} → Const τ → FExpr Γ τ
    var    : {n : ℕ} {Γ : Sig n} → (i : Fin n) → FExpr Γ (lookup i Γ)
    dist   : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ τ₁ → FExpr Γ (list τ₂) → FExpr Γ (list τ₁)
    ldist  : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (↑ τ₁) → FExpr Γ (↑ (list τ₂)) → FExpr Γ (↑ (list τ₁))
    conc   : {n : ℕ} {τ : Type} {Γ : Sig n} → FExpr Γ (list (list τ)) → FExpr Γ (list τ)
    unconc : {n : ℕ} {τ₁ τ₂ : Type} {Γ : Sig n} → FExpr Γ (list (list τ₁)) → FExpr Γ (list τ₂) → FExpr Γ (list (list τ₂))

{-
  data Tele : {n : ℕ} → Sig n → Set where
    []  : Tele []
    _∷_ : {n : ℕ} → {Γ : Sig n} {τ : Type} → FExpr Γ τ → Tele Γ → Tele (τ ∷ Γ)
-}

{-
teleLookup : {n : ℕ} {Γ : Sig n } → (i : Fin n) → Tele Γ → FExpr {!!} (lookup i Γ)
teleLookup = {!!} 
-}

interpretConst : {τ : Type} → Const τ → ⟦ τ ⟧
interpretConst (num n) = n
interpretConst (list cs) = L.map interpretConst cs


interpret : {n : ℕ} {Γ : Sig n} {τ : Type} → Env Γ → FExpr Γ τ → F⟦ τ ⟧
interpret env (clos {n} {τ₁} {τ₂} {Γ} e₁ e₂)  = ( (λ x → interpret (x ∷ env) e₁) , (λ {τ} x ys → 
                  interpret {suc (suc n)} {list τ ∷ list τ₁ ∷ map list Γ}
                            (ys ∷ (x ∷ envMap list (λ {σ} z → interpret {suc (suc zero)} {σ ∷ list τ ∷ []} {list σ} (z ∷ ys ∷ []) (dist (var zero) (var (suc zero)))) env)) e₂) )
  -- (λ x → λ ys → interpret (ys ∷ (x ∷ envMap list (λ z → interpret (z ∷ ys ∷ []) (dist (var zero) (var (suc zero)))) env)) e₂) )
interpret env e = {!!}
{-
interpret env (capp e₁ e₂)  = interpret env e₁ (interpret env e₂)
interpret env (lapp e₁ e₂)  = L.zipWith id (interpret env e₁) (interpret env e₂)
interpret env (clos e₁ e₂)  = λ x → interpret (x ∷ env) e₁
interpret env (lclos e₁ e₂) = {!!}
interpret env (add e₁ e₂)   = interpret env e₁ + interpret env e₂
interpret env (ladd e₁ e₂)  = L.zipWith _+_ (interpret env e₁) (interpret env e₂)
interpret env (con c)       = interpretConst c
interpret env (var i)       = envLookup i env
interpret env (dist e₁ e₂)  = L.replicate (length (interpret env e₂)) (interpret env e₁) -- !!!
interpret env (ldist y y')  = {!!}
interpret env (conc e)      = L.concat (interpret env e)
interpret env (unconc y y') = {!!}
-}

lemma : {n : ℕ} {i : Fin n} {Γ : Sig n} → lookup i (↑Sig Γ) ≡ ↑ (lookup i Γ)
lemma {zero} {()} {[]}
lemma {suc n} {zero}  {x ∷ xs} = refl
lemma {suc n} {suc i} {x ∷ xs} = lemma {n} {i} {xs}

mutual
  ↑Expr : {n : ℕ} {Γ : Sig n} {σ τ : Type} → Expr Γ τ → FExpr (list σ ∷ ↑Sig Γ) (↑ τ)
  ↑Expr (con c)         = dist (con c) (var zero)
  ↑Expr (app e₁ e₂)     = lapp (↑Expr e₁) (↑Expr e₂)
  ↑Expr (lam e)         = lclos (flat e) (↑Expr e)
  ↑Expr (add e₁ e₂)     = ladd (↑Expr e₁) (↑Expr e₂)
  ↑Expr (cmap e₁ e₂)    = unconc (↑Expr e₂) (lapp (conc (ldist (dist {!flat e₁!} {!!}) (↑Expr e₂))) (conc (↑Expr e₂)))
                       -- unconc (↑Expr e₂) (lapp (conc (ldist (↑Expr e₁) (↑Expr e₂))) (conc (↑Expr e₂))) -- !!!
  ↑Expr {n} {Γ} (var i) = subst (FExpr _) (lemma {n} {i} {Γ}) (var (suc i))

  flat : {n : ℕ} {Γ : Sig n} {τ : Type} → Expr Γ τ → FExpr Γ τ
  flat (app e₁ e₂)  = capp (flat e₁) (flat e₂)
  flat (lam e)      = clos (flat e) (↑Expr e)
  flat (add e₁ e₂)  = add (flat e₁) (flat e₂)
  flat (con c)      = con c
  flat (cmap e₁ e₂) = lapp (dist (flat e₁) (flat e₂)) (flat e₂)
  flat (var i)      = var i
