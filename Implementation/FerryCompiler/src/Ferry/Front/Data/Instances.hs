module Ferry.Front.Data.Instances where
    
import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta
import Ferry.Syntax(VarContainer(..))

instance HasMeta Expr where              
--    getMeta (UnOp         m _ _)         = m
    getMeta (BinOp        m _ _ _)       = m
    getMeta (Const        m _)           = m
    getMeta (Var          m _)           = m
    getMeta (App          m _ _)         = m
    getMeta (If           m _ _ _)       = m
    getMeta (Record       m _)           = m
    getMeta (Paren        m _)           = m
    getMeta (List         m _)           = m
    getMeta (Elem         m _ _)         = m
    getMeta (Let          m _ _)         = m
    getMeta (Table        m _ _ _)       = m
    getMeta (Relationship m _ _ _ _ _ _) = m 
    getMeta (QComp        m _)           = m
    getMeta (Lookup m _ _)               = m

instance HasMeta QCompr where
    getMeta (FerryCompr m _ _ _) = m
    getMeta (HaskellCompr m _ _) = m
    
instance VarContainer Pattern where
    vars (PVar _ v)  = [v]
    vars (PPat _ vs) = vs
    
instance VarContainer BodyElem where
    vars (For _ b)    = concatMap vars $ map fst b
    vars (ForLet _ b) = concatMap vars $ map fst b
    vars (ForWhere _ _) = []
    vars (ForOrder _ _) = []
    vars (Group _ _ _ _ _) = []

