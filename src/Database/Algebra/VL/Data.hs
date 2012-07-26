{-# LANGUAGE RankNTypes, GADTs, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
module Database.Algebra.VL.Data where

import GHC.Generics (Generic)

import Database.Algebra.Aux
import Database.Algebra.Dag(Operator, replaceOpChild, opChildren)
import Database.Algebra.Dag.Common

type VL = Algebra TerOp BinOp UnOp NullOp AlgNode

data VLType = Nat | Int | Bool |  Double
            | String | Unit
            | Pair VLType VLType | VLList VLType
            deriving (Eq, Ord, Generic, Show)

type DataColumn = String 
type TypedColumn = (DataColumn, VLType)
type Key = [DataColumn]
type DBCol = Int

data VecOp = Add 
           | Sub 
           | Div 
           | Mul 
           | Mod 
           | Eq  
           | Gt  
           | GtE 
           | Lt  
           | LtE 
           | Cons 
           | Conj 
           | Disj 
           deriving (Eq, Ord, Generic)
    
instance Show VecOp where
    show Add = "+"
    show Sub = "-"
    show Div = "/"
    show Mul = "*"
    show Mod = "%"
    show Eq  = "=="
    show Gt  = ">"
    show GtE = ">="
    show Lt  = "<"
    show LtE = "<="
    show Cons = ":"
    show Conj = "&&"
    show Disj = "||"
    
{-
data Projection = Number
                | ConstCol VLVal
                | Payload Int
                | DescrCol
                | PosCol
                | PosNewCol
                | PosOldCol
                deriving (Eq, Ord, Generic, Show)
-}
    
data ISTransProj = STDescrCol
                 | STPosCol
                 | STNumber
                 deriving (Eq, Ord, Generic, Show)
                         
data DescrProj = DescrConst VLVal
               | DescrIdentity
               | DescrPosCol
               deriving (Eq, Ord, Generic, Show)
                        
data PosProj = PosNumber
             | PosConst VLVal
             | PosIdentity
             deriving (Eq, Ord, Generic, Show)
                         
data PayloadProj = PLNumber
                 | PLConst VLVal
                 | PLCol DBCol
                 deriving (Eq, Ord, Generic, Show)
                          
data VLVal = VLInt Int
           | VLNat Int
           | VLBool Bool
           | VLString String
           | VLDouble Double
           | VLUnit
           deriving (Eq, Ord, Generic, Show)

data NullOp = SingletonDescr
            | ConstructLiteralValue [VLType] [VLVal]
            | ConstructLiteralTable [VLType] [[VLVal]]
            | TableRef String [TypedColumn] [Key]
            deriving (Eq, Ord, Generic, Show)

data UnOp = Unique
          | UniqueL
          | NotPrim
          | NotVec
          | LengthA
          | DescToRename
          | ToDescr
          | Segment
          | VecSum VLType
          | VecMin
          | VecMinL
          | VecMax
          | VecMaxL
          | ProjectL [DBCol]
          | ProjectA [DBCol]
          | IntegerToDoubleA
          | IntegerToDoubleL
          | ReverseA -- (DBV, PropVector)
          | ReverseL -- (DBV, PropVector)
          | FalsePositions
          | R1 
          | R2
          | R3
          | ProjectRename (ISTransProj, ISTransProj)
          | ProjectValue (DescrProj, PosProj, [PayloadProj])
          | SelectItem
          | Only
          | Singleton
          | VecBinOpSingle (VecOp, DBCol, DBCol)
    deriving (Eq, Ord, Generic, Show)

data BinOp = GroupBy    -- (DescrVector, DBV, PropVector)
           | SortWith   -- (DBV, PropVector)
           | LengthSeg
           | DistPrim   -- (DBV, PropVector)
           | DistDesc   -- (DBV, PropVector)
           | DistLift   -- (DBV, PropVector)
           | PropRename
           | PropFilter -- (DBV, PropVector)
           | PropReorder -- (DBV, PropVector)
           | Append     -- (DBV, RenameVector, RenameVector)
           | RestrictVec -- VL (DBV, RenameVector)
           | VecBinOp VecOp
           | VecBinOpL VecOp
           | VecSumL
           | SelectPos VecOp -- (DBV, RenameVector)
           | SelectPosL VecOp -- (DBV, RenameVector)
           | PairA
           | PairL
           | ZipL       -- (DBV, RenameVector, RenameVector)
           | CartProduct -- DBV
           | ThetaJoin (VecOp, DBCol, DBCol)
    deriving (Eq, Ord, Generic, Show)
    
data TerOp = CombineVec  -- (DBV, RenameVector, RenameVector)
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
