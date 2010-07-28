module Ferry.Front.Render.Pretty where
    
import Ferry.Front.Data.Base
import Ferry.Front.Data.Language
import Ferry.Common.Render.Pretty

import qualified Data.List as L

prettyAST :: Expr -> String
prettyAST e = prettyPrint e
    
instance Pretty Expr where
--    pretty (UnOp         _ o e)               i = (pretty o i) ++ (pretty e i)
    pretty (BinOp        _ o e1 e2)           i = (pretty e1 i) ++ " " ++ (pretty o i) ++ " " ++ (pretty e2 i)
    pretty (Const        _ c)                 i = pretty c i
    pretty (Var          _ i)                 _ = i
    pretty (App          _ e1 e2)             i = (pretty e1 i) ++ " " ++ (pretty e2 i)
    pretty (If           _ e1 e2 e3)          i = "if " ++ pretty e1 i ++
                                                    (newLine $ i + 3) ++ "then " ++ (pretty e2 $ i + 8) ++
                                                    (newLine $ i + 3) ++ "else " ++ (pretty e3 $ i + 8)
    pretty (Record       _ elems)             i = case head elems of
                                                    (TrueRec _ _ _) -> "{" ++ (mapIntersperseConcat (flip pretty i) ", " elems) ++"}"
                                                    (TuplRec _ _ _) -> "(" ++ (mapIntersperseConcat (flip pretty i) ", " elems) ++")"
    pretty (Paren        _ e)                 i = "(" ++ (pretty e i) ++ ")"
    pretty (List         _ es)                i = "[" ++ (mapIntersperseConcat (flip pretty i) ", " es) ++ "]"
    pretty (Elem         _ e (Left s))        i = pretty e i ++ "." ++ s
    pretty (Elem         _ e (Right s))       i = pretty e i ++ "." ++ show s
    pretty (Lookup       _ e1 e2)             i = pretty e1 i ++ "<" ++ pretty e2 i ++ ">"
    pretty (Let          _ bs e)              i = let body = mapIntersperseConcat (flip pretty $ i + 4) ((:) ',' $ newLine (i + 4)) bs
                                                   in "let " ++ body ++ (newLine i) ++ " in " ++ pretty e (i + 4)                                                     
    pretty (Table        _ n cs ks)           i = "table " ++ n ++ (mapIntersperseConcat (flip pretty i) ", " cs) ++
                                                   newLine (i + 1) ++ "with keys (" ++ (mapIntersperseConcat (flip pretty i) ", " cs) ++ ")"
    pretty (Relationship _ c1 e1 c2 e2 k1 k2) i = "relationship from " ++ pretty c1 i ++ " " ++ pretty e1 i ++
                                                    newLine (i+15) ++ "to " ++ pretty c2 (i + 18) ++ " " ++ pretty e2 (i + 18) ++
                                                    newLine (i+15) ++ "by " ++ pretty k1 (i + 18) ++ " eq " ++ pretty k2 (i + 18)
    pretty (QComp        _ comp)              i = pretty comp i

instance Pretty a => Pretty [a] where
    pretty (x:xs) i = pretty x i ++ " " ++ pretty xs i
    pretty [] i = ""
    
instance Pretty Arg where
    pretty (AExpr _ e) i = pretty e i
    pretty (AAbstr _ x e) i = "(\\" ++ pretty x i ++ " ->" ++ pretty e i ++ ")"
    
instance Pretty Op where
    pretty (Op _ o) _ = o
    
instance Pretty RecElem where
    pretty (TrueRec _ n e) i = case n of
                                Left ex -> pretty ex i
                                Right s -> s ++ case e of
                                                    Nothing -> ""
                                                    Just e ->  " = " ++ pretty e i
    pretty (TuplRec _ _ e) i = pretty e i
    
instance Pretty Type where
    pretty (TInt _) _ = "Int"
    pretty (TFloat _) _ = "Float"
    pretty (TString _) _ = "String"
    pretty (TBool _) _ = "Bool"
    
instance Pretty Order where
    pretty (Ascending _) _ = "ascending"
    pretty (Descending _) _ = "descending"
    
instance Pretty Cardinality where
    pretty (One _) _ = "one"
    pretty (Many _) _ = "many"
    
instance Pretty Pattern where
    pretty (PVar _ x)  _ = x
    pretty (PPat _ xs) _ = "(" ++ (concat $ L.intersperse ", " xs ) ++ ")"
    
instance Pretty Key where
    pretty  (Key _ k) _ = "(" ++ (concat $ L.intersperse ", " k ) ++ ")"   
    
instance Pretty Column where
    pretty (Column _ n t) i = n ++ (pretty t i)
    
instance Pretty Binding where
    pretty (Binding _ x e) i = x ++ " = " ++ (pretty e (i + (length x) + 3))
    
instance Pretty ExprOrder where
    pretty (ExprOrder _ e o) i = pretty e i ++ " " ++ pretty o i
    
instance Pretty QCompr where
    pretty (FerryCompr _ bs bd r) i = "for " ++ mapIntersperseConcat (binds i) ((:) ',' $ newLine (i + 4)) bs
                                             ++ (newLine $ i + 4) ++
                                             mapIntersperseConcat (flip pretty (i + 4)) (newLine $ i + 4) bd ++
                                             newLine i ++ pretty r (i + 7)
    pretty (HaskellCompr _ _ _) _ = error "HaskellCompr cannot be pretty printed"

instance Pretty ReturnElem where
    pretty (Return _ e c) i = "return " ++ pretty e (i + 7) ++
                                case c of
                                    Nothing -> ""
                                    Just (p, l, r) -> " into " ++ pretty p (i + 7) ++ newLine (i + 7) ++ 
                                                        mapIntersperseConcat (flip pretty (i + 7)) (newLine $ i + 7) l ++
                                                        newLine (i + 7) ++ "return " ++ pretty r (i + 14)

binds :: Int -> (Pattern, Expr) -> String
binds i (p, e) = pretty p i ++ " in " ++ pretty e (i+ (length (pretty p 0)) + 4)

lets :: Int -> (Pattern, Expr) -> String
lets i (p, e) = pretty p i ++ " = " ++ pretty e (i+ (length (pretty p 0)) + 3)

instance Pretty BodyElem where
    pretty (For _ ps) i = "for " ++ mapIntersperseConcat (binds i) ((:) ',' $ newLine (i + 4)) ps
    pretty (ForLet _ ps) i = "let " ++ mapIntersperseConcat (lets i) ((:) ',' $ newLine (i + 4)) ps
    pretty (ForWhere _ e) i = "where " ++ pretty e (i+6)
    pretty (ForOrder _ os) i = "order by " ++ mapIntersperseConcat (flip pretty (i+9)) ", " os
    pretty (Group _ g e es p) i = let mid = case g of
                                                GBy -> "by "
                                                GWith -> "with "
                                      into = case p of
                                                   Nothing -> ""
                                                   Just p  -> newLine (i+4) ++ "into " ++ pretty p (i+9)
                                   in
                                    "group " ++ (case e of
                                                Nothing -> ""
                                                Just e -> pretty e (i+6) ++ newLine(i+1))  ++ mid ++ intersperseComma es (i+9) ++ into

instance Pretty Const where
    pretty (CInt i) _ = (show i)
    pretty (CFloat d) _ = (show d)
    pretty (CBool b) _ = (show b)
    pretty (CString s) _ = "\"" ++ s ++ "\""    