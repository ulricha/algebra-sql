module Ferry.TypedCore.Render.Dot where


import Ferry.Common.Render.Dot
import Ferry.Common.Render.Pretty    
import Ferry.TypedCore.Data.TypedCore
import Ferry.TypedCore.Data.Type
import Ferry.Front.Data.Base
import Ferry.TypedCore.Render.Pretty

import qualified Data.List as L



-- type Dot = ErrorT FerryError (WriterT [Node] (WriterT [Edge] (State Int)))

toDot :: CoreExpr -> Dot Id
toDot (BinOp t o e1 e2) = do
                          nId <- getFreshId
                          tId <- typeToDot t
                          id1 <- toDot e1
                          id2 <- toDot e2
                          let o' = (\(Op o) -> o) o
                          node nId [Label $ SLabel o', Color Green, Shape Circle]
                          edge nId [id1, id2, tId]
                          return nId
{- toDot (UnaOp t o e) = do
                      nId <- getFreshId
                      tId <- typeToDot t
                      eId <- toDot e
                      let o' = (\(Op o) -> o) o
                      node nId [Label $ SLabel o', Color Green, Shape Circle]
                      edge nId [eId, tId]
                      return nId -}
toDot (Constant t c) = do
                      nId <- getFreshId
                      tId <- typeToDot t
                      let s = toString c
                      node nId [Label $ SLabel s, Color Yellow, Shape Triangle]
                      edge nId [tId]
                      return nId
toDot (Var t i) = do
                    nId <- getFreshId
                    tId <- typeToDot t
                    edge nId [tId]
                    node nId [Label $ SLabel i, Color Red, Shape Triangle]
                    return nId
toDot (App t c ps) = do
                     nId <- getFreshId
                     tId <- typeToDot t
                     fId <- toDot c
                     pIds <- paramToDot ps
                     node nId [Label $ SLabel "$", Color Green, Shape Circle]
                     edge nId [fId, pIds, tId]
                     return nId
toDot (Let t s e1 e2) = do
                       nId <- getFreshId
                       tId <- typeToDot t
                       id0 <- getFreshId
                       id1 <- toDot e1
                       id2 <- toDot e2
                       node id0 [Label $ SLabel s, Color Red, Shape Rect, TextColor White]
                       node nId [Label $ SLabel "Let", Color Blue, Shape Rect]
                       edge nId [id0, id1, id2, tId]
                       return nId
toDot (Rec t es) = do
                  nId <- getFreshId
                  tId <- typeToDot t
                  eIds <- mapM recToDot es
                  node nId [Label $ SLabel "Rec", Color Blue, Shape Oval]
                  edge nId (eIds ++ [tId])
                  return nId
toDot (Cons t e1 e2) = do
                     nId <- getFreshId
                     tId <- typeToDot t
                     eIdh <- toDot e1
                     eIdt <- toDot e2
                     node nId [Label $ SLabel "Cons", Color Blue, Shape Oval]
                     edge nId [eIdh, eIdt, tId]
                     return nId
toDot (Nil t)      = do
                    nId <- getFreshId
                    tId <- typeToDot t
                    node nId [Label $ SLabel "Nil", Color Blue, Shape Oval]
                    edge nId [tId]
                    return nId
toDot (Elem t c s) = do
                    nId <- getFreshId
                    sId <- getFreshId
                    tId <- typeToDot t
                    node nId [Label $ SLabel ".", Color Green, Shape Circle]
                    node sId [Label $ SLabel s, Color Red, Shape Triangle]
                    cId <- toDot c
                    edge nId [cId, sId, tId]
                    return nId
toDot (Table ty n cs ks) = do
                         nId <- getFreshId
                         tId <- typeToDot ty
                         let label = VLabel $ ((HLabel [SLabel "Table:", SLabel n])
                                            : [HLabel [SLabel $ n ++ "::", SLabel $ prettyPrint t ] | (Column n t) <- cs])
                                            ++ [SLabel $ keyToString k | k <- ks]
                         node nId [Shape Rect, Label label, Color Yellow]
                         edge nId [tId]
                         return nId
toDot (If t e1 e2 e3) = do
                        nId <- getFreshId
                        tId <- typeToDot t
                        eId1 <- toDot e1
                        eId2 <- toDot e2
                        eId3 <- toDot e3
                        node nId [Label $ SLabel "If", Color Blue, Shape Circle]
                        edge nId [eId1, eId2, eId3, tId]
                        return nId
                        

paramToDot :: Param -> Dot Id
paramToDot (ParExpr t e) = toDot e
paramToDot (ParAbstr t p e) = do
                             nId <- getFreshId
                             tId <- typeToDot t
                             pId <- patToDot p
                             eId <- toDot e
                             node nId [Label $ SLabel "\\   ->", Color Blue, Shape Circle]
                             edge nId [pId, eId, tId]
                             return nId
                             
patToDot :: Pattern -> Dot Id
patToDot (PVar s) = do
                     nId <- getFreshId
                     node nId [Label $ SLabel s, Color Red, Shape Triangle]
                     return nId
patToDot (Pattern s) = do
                        nId <- getFreshId
                        node nId [Label $ SLabel $  "(" ++ (concat $ L.intersperse ", " s) ++ ")", Color Red, Shape Triangle]
                        return nId
                        
recToDot (RecElem t s e) = do
                          nId <- getFreshId
                          tId <- typeToDot t
                          eId <- toDot e
                          node nId [Label $ SLabel s, Color Red, Shape Oval]
                          edge nId [eId, tId]
                          return nId

keyToString :: Key -> String
keyToString (Key ks) = "(" ++ (concat $ L.intersperse ", " ks) ++ ")"

typeToDot :: Qual FType -> Dot Id
typeToDot t = do
               nId <- getFreshId
               node nId [Label $ SLabel $ prettyPrint t, Color Gray, Shape Rect]
               return nId