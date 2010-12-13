{- | Transform a typed core AST into a dot graph -}
module Ferry.TypedCore.Render.Dot where

import Ferry.Common.Render.Dot
import Ferry.Common.Render.Pretty    
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type
import Ferry.Common.Data.Base
import Ferry.TypedCore.Render.Pretty()

import qualified Data.List as L

toDot :: CoreExpr -> Dot Id
toDot (BinOp t o e1 e2) = do
                           let o' = (\(Op op) -> op) o  
                           nId <- node [Label $ SLabel o', Color Green, Shape Circle]
                           tId <- typeToDot t
                           id1 <- toDot e1
                           id2 <- toDot e2
                           edge nId [id1, id2, tId]
                           return nId
toDot (Constant t c) = do
                      let s = toString c
                      nId <- node [Label $ SLabel s, Color Yellow, Shape Triangle]
                      tId <- typeToDot t
                      edge nId [tId]
                      return nId
toDot (Var t i) = do
                    nId <- node [Label $ SLabel i, Color Red, Shape Triangle]
                    tId <- typeToDot t
                    edge nId [tId]
                    return nId
toDot (App t c ps) = do
                     nId <- node [Label $ SLabel "$", Color Green, Shape Circle]
                     tId <- typeToDot t
                     fId <- toDot c
                     pIds <- paramToDot ps
                     edge nId [fId, pIds, tId]
                     return nId
toDot (Let t s e1 e2) = do
                       nId <- node [Label $ SLabel "Let", Color Blue, Shape Rect]
                       tId <- typeToDot t
                       id0 <- node [Label $ SLabel s, Color Red, Shape Rect, TextColor White]
                       id1 <- toDot e1
                       id2 <- toDot e2
                       edge nId [id0, id1, id2, tId]
                       return nId
toDot (Rec t es) = do
                  nId <- node [Label $ SLabel "Rec", Color Blue, Shape Oval]
                  tId <- typeToDot t
                  eIds <- mapM recToDot es
                  edge nId (eIds ++ [tId])
                  return nId
toDot (Cons t e1 e2) = do
                     nId <- node [Label $ SLabel "Cons", Color Blue, Shape Oval]
                     tId <- typeToDot t
                     eIdh <- toDot e1
                     eIdt <- toDot e2
                     edge nId [eIdh, eIdt, tId]
                     return nId
toDot (Nil t)      = do
                    nId <- node [Label $ SLabel "Nil", Color Blue, Shape Oval]
                    tId <- typeToDot t
                    edge nId [tId]
                    return nId
toDot (Elem t c s) = do
                    nId <- node [Label $ SLabel ".", Color Green, Shape Circle]
                    sId <- node [Label $ SLabel s, Color Red, Shape Triangle]
                    tId <- typeToDot t
                    cId <- toDot c
                    edge nId [cId, sId, tId]
                    return nId
toDot (Table ty n cs ks) = do
                         let label = VLabel $ ((HLabel [SLabel "Table:", SLabel n])
                                            : [HLabel [SLabel $ n' ++ "::", SLabel $ prettyPrint t ] | (Column n' t) <- cs])
                                            ++ [SLabel $ keyToString k | k <- ks]
                         nId <- node [Shape Rect, Label label, Color Yellow]
                         tId <- typeToDot ty
                         edge nId [tId]
                         return nId
toDot (If t e1 e2 e3) = do
                        nId <- node [Label $ SLabel "If", Color Blue, Shape Circle]
                        tId <- typeToDot t
                        eId1 <- toDot e1
                        eId2 <- toDot e2
                        eId3 <- toDot e3
                        edge nId [eId1, eId2, eId3, tId]
                        return nId
                        

paramToDot :: Param -> Dot Id
paramToDot (ParExpr _ e) = toDot e
paramToDot (ParAbstr t p e) = do
                             nId <- node [Label $ SLabel "\\   ->", Color Blue, Shape Circle]
                             tId <- typeToDot t
                             pId <- patToDot p
                             eId <- toDot e
                             edge nId [pId, eId, tId]
                             return nId
                             
patToDot :: Pattern -> Dot Id
patToDot (PVar s) = node [Label $ SLabel s, Color Red, Shape Triangle]
patToDot (Pattern s) = node [Label $ SLabel $  "(" ++ (concat $ L.intersperse ", " s) ++ ")", Color Red, Shape Triangle]
                        
recToDot :: RecElem -> Dot Id
recToDot (RecElem t s e) = do
                          nId <- node [Label $ SLabel s, Color Red, Shape Oval]
                          tId <- typeToDot t
                          eId <- toDot e
                          edge nId [eId, tId]
                          return nId

keyToString :: Key -> String
keyToString (Key ks) = "(" ++ (concat $ L.intersperse ", " ks) ++ ")"

typeToDot :: Qual FType -> Dot Id
typeToDot t = node [Label $ SLabel $ prettyPrint t, Color Gray, Shape Rect]