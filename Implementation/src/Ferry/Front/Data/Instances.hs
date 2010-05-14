module Ferry.Front.Data.Instances where
    
import Ferry.Front.Data.Language
import Ferry.Front.Data.Meta
import Ferry.Front.Data.Base

instance HasMeta Expr where              
    getMeta (UnOp         m _ _)         = m
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

instance HasMeta QCompr where
    getMeta (FerryCompr m _ _ _) = m
    getMeta (HaskellCompr m)       = m
    
