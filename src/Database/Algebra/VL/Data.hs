{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Algebra.VL.Data where

import           GHC.Generics                (Generic)

import           Database.Algebra.Aux
import           Database.Algebra.Dag        (Operator, opChildren, replaceOpChild)
import           Database.Algebra.Dag.Common

type VL = Algebra TerOp BinOp UnOp NullOp AlgNode

data VLType = Nat | Int | Bool |  Double
            | String | Unit
            | Pair VLType VLType | VLList VLType
            deriving (Eq, Ord, Generic, Show, Read)

type DataColumn = String
type TypedColumn = (DataColumn, VLType)
type Key = [DataColumn]
type DBCol = Int

data AggrFun = AggrSum VLType Expr1
             | AggrMin Expr1
             | AggrMax Expr1
             | AggrAvg Expr1
             | AggrCount
               deriving (Eq, Ord, Show, Read, Generic)

data VecCompOp = Eq
               | Gt
               | GtE
               | Lt
               | LtE
               deriving (Eq, Ord, Generic, Read)

data VecNumOp = Add
              | Sub
              | Div
              | Mul
              | Mod
              deriving (Eq, Ord, Generic, Read)

data VecBoolOp = Conj
               | Disj
               deriving (Eq, Ord, Generic, Read)
               
data VecCastOp = CastDouble
               deriving (Eq, Ord, Generic, Read)

data VecOp = COp VecCompOp
           | NOp VecNumOp
           | BOp VecBoolOp
           | Like
           deriving (Eq, Ord, Generic, Read)
           
data VecUnOp = Not 
             | CastOp VecCastOp
             deriving (Eq, Ord, Generic, Read)

data Expr1 = BinApp1 VecOp Expr1 Expr1
           | UnApp1 VecUnOp Expr1
           | Column1 DBCol
           | Constant1 VLVal
           deriving (Eq, Ord, Show, Generic, Read)

newtype LeftCol = L DBCol deriving (Eq, Ord, Show, Generic)
newtype RightCol = R DBCol deriving (Eq, Ord, Show, Generic)

data Expr2 = BinApp2 VecOp Expr2 Expr2
           | UnApp2 VecUnOp Expr2
           | Column2Left LeftCol
           | Column2Right RightCol
           | Constant2 VLVal
           deriving (Eq, Ord, Show, Generic)

instance Show VecOp where
    show (NOp Add)  = "+"
    show (NOp Sub)  = "-"
    show (NOp Div)  = "/"
    show (NOp Mul)  = "*"
    show (NOp Mod)  = "%"
    show (COp o)    = show o
    show (BOp Conj) = "&&"
    show (BOp Disj) = "||"
    show Like       = "LIKE"

instance Show VecCompOp where
    show Eq  = "=="
    show Gt  = ">"
    show GtE = ">="
    show Lt  = "<"
    show LtE = "<="
    
instance Show VecUnOp where
    show Not = "not"
    show (CastOp CastDouble) = "double"

data ISTransProj = STDescrCol
                 | STPosCol
                 | STNumber
                 deriving (Eq, Ord, Generic, Show)

newtype Nat = N Int deriving (Eq, Ord, Generic, Show, Read)

instance Integral Nat where
  quot (N i1) (N i2)    = N $ quot i1 i2
  rem (N i1) (N i2)     = N $ rem i1 i2
  div (N i1) (N i2)     = N $ div i1 i2
  mod (N i1) (N i2)     = N $ mod i1 i2
  quotRem (N i1) (N i2) = let (q, r) = quotRem i1 i2 in (N q, N r)
  divMod (N i1) (N i2)  = let (d, m) = divMod i1 i2 in (N d, N m)
  toInteger (N i)       = toInteger i

instance Real Nat where
  toRational (N i) = toRational i

instance Enum Nat where
  toEnum         = N
  fromEnum (N i) = i

instance Num Nat where
  (N i1) + (N i2) = N $ i1 + i2
  (N i1) * (N i2) = N $ i1 * i2
  (N i1) - (N i2) = N $ i1 - i2
  negate (N i)    = N $ negate i
  abs (N i)       = N $ abs i
  signum (N i)    = N $ signum i
  fromInteger i   = N $ fromInteger i

data VLVal = VLInt Int
           | VLNat Nat
           | VLBool Bool
           | VLString String
           | VLDouble Double
           | VLUnit
           deriving (Eq, Ord, Generic, Show, Read)

data NullOp = SingletonDescr
            | Lit [VLType] [[VLVal]]
            | TableRef String [TypedColumn] [Key]
            deriving (Eq, Ord, Generic, Show)

data UnOp = UniqueS
          | Number
          | NumberS
          | DescToRename
          | Segment
          | Unsegment
          | Reverse -- (DBV, PropVector)
          | ReverseS -- (DBV, PropVector)
          | FalsePositions
          | R1
          | R2
          | R3
          | ProjectRename (ISTransProj, ISTransProj) -- (source, target)?
          | Project [Expr1]
          | Select Expr1
          | Only
          | Singleton
          | SelectPos1 VecCompOp Nat
          | SelectPos1S VecCompOp Nat
          | GroupAggr [Expr1] [AggrFun]
          | Aggr AggrFun
          | SortSimple [Expr1]
          | GroupSimple [Expr1]
          | Reshape Integer
          | ReshapeS Integer
    deriving (Eq, Ord, Generic, Show)

data BinOp = GroupBy    -- (DescrVector, DBV, PropVector)
           | Sort        -- (DBV, PropVector)
           | AggrS AggrFun
           | DistPrim   -- (DBV, PropVector)
           | DistDesc   -- (DBV, PropVector)
           | DistSeg   -- (DBV, PropVector)
           | PropRename
           | PropFilter -- (DBV, PropVector)
           | PropReorder -- (DBV, PropVector)
           | Append     -- (DBV, RenameVector, RenameVector)
           | Restrict -- VL (DBV, RenameVector)
           | BinExpr Expr2
           | SelectPos VecCompOp -- (DBV, RenameVector)
           | SelectPosS VecCompOp -- (DBV, RenameVector)
           | Zip
           | ZipS            -- (DBV, RenameVector, RenameVector)
           | CartProduct
           | CartProductS
           | EquiJoin Expr1 Expr1
           | EquiJoinS Expr1 Expr1
           | SemiJoin Expr1 Expr1
           | SemiJoinS Expr1 Expr1
           | AntiJoin Expr1 Expr1
           | AntiJoinS Expr1 Expr1
           | Transpose
           | NestJoinS Expr1 Expr1
           | NestProductS
    deriving (Eq, Ord, Generic, Show)

data TerOp = Combine  -- (DBV, RenameVector, RenameVector)
           | TransposeS
    deriving (Eq, Ord, Generic, Show)

instance Operator VL where
    opChildren (TerOp _ c1 c2 c3) = [c1, c2, c3]
    opChildren (BinOp _ c1 c2) = [c1, c2]
    opChildren (UnOp _ c) = [c]
    opChildren (NullaryOp _) = []

    replaceOpChild oper old new = replaceChild old new oper
     where
         replaceChild :: forall t b u n c. Eq c => c -> c -> Algebra t b u n c -> Algebra t b u n c
         replaceChild o n (TerOp op c1 c2 c3) = TerOp op (replace o n c1) (replace o n c2) (replace o n c3)
         replaceChild o n (BinOp op c1 c2) = BinOp op (replace o n c1) (replace o n c2)
         replaceChild o n (UnOp op c) = UnOp op (replace o n c)
         replaceChild _ _ (NullaryOp op) = NullaryOp op
