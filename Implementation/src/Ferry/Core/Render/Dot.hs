module Ferry.Core.Render.Dot where


import Ferry.Common.Render.Dot    
import Ferry.Core.Data.Core
import Ferry.Front.Data.Base
import Ferry.Core.Render.Pretty

import qualified Data.List as L



-- type Dot = ErrorT FerryError (WriterT [Node] (WriterT [Edge] (State Int)))

toDot :: CoreExpr -> Dot Id
toDot (BinOp o e1 e2) = do
                          id1 <- toDot e1
                          id2 <- toDot e2
                          let o' = (\(Op o) -> o) o
                          nId <- node [Label $ SLabel o', Color Green, Shape Circle]
                          edge nId [id1, id2]
                          return nId
{- toDot (UnaOp o e) = do
                      nId <- getFreshId
                      eId <- toDot e
                      let o' = (\(Op o) -> o) o
                      node nId [Label $ SLabel o', Color Green, Shape Circle]
                      edge nId [eId]
                      return nId -}
toDot (Constant c) = do
                      nId <- getFreshId
                      let s = toString c
                      node nId [Label $ SLabel s, Color Yellow, Shape Triangle]
                      return nId
toDot (Var i) = do
                    nId <- getFreshId
                    node nId [Label $ SLabel i, Color Red, Shape Triangle]
                    return nId
toDot (App c ps) = do
                     nId <- getFreshId
                     fId <- toDot c
                     pIds <- paramToDot ps
                     node nId [Label $ SLabel "$", Color Green, Shape Circle]
                     edge nId [fId, pIds]
                     return nId
toDot (Let s e1 e2) = do
                       nId <- getFreshId
                       id0 <- getFreshId
                       id1 <- toDot e1
                       id2 <- toDot e2
                       node id0 [Label $ SLabel s, Color Red, Shape Rect, TextColor White]
                       node nId [Label $ SLabel "Let", Color Blue, Shape Rect]
                       edge nId [id0, id1, id2]
                       return nId
toDot (Rec es) = do
                  nId <- getFreshId
                  eIds <- mapM recToDot es
                  node nId [Label $ SLabel "Rec", Color Blue, Shape Oval]
                  edge nId eIds
                  return nId
toDot (Cons e1 e2) = do
                     nId <- getFreshId
                     eIdh <- toDot e1
                     eIdt <- toDot e2
                     node nId [Label $ SLabel "Cons", Color Blue, Shape Oval]
                     edge nId [eIdh, eIdt]
                     return nId
toDot (Nil)      = do
                    nId <- getFreshId
                    node nId [Label $ SLabel "Nil", Color Blue, Shape Oval]
                    return nId
toDot (Elem c s) = do
                    nId <- getFreshId
                    sId <- getFreshId
                    node nId [Label $ SLabel ".", Color Green, Shape Circle]
                    node sId [Label $ SLabel s, Color Red, Shape Triangle]
                    cId <- toDot c
                    edge nId [cId, sId]
                    return nId
toDot (Table n cs ks) = do
                         nId <- getFreshId
                         let label = VLabel $ ((HLabel [SLabel "Table:", SLabel n])
                                            : [HLabel [SLabel $ n ++ "::", SLabel $ prettyTy t ] | (Column n t) <- cs])
                                            ++ [SLabel $ keyToString k | k <- ks]
                         node nId [Shape Rect, Label label, Color Yellow]
                         return nId
toDot (If e1 e2 e3) = do
                        nId <- getFreshId
                        eId1 <- toDot e1
                        eId2 <- toDot e2
                        eId3 <- toDot e3
                        node nId [Label $ SLabel "If", Color Blue, Shape Circle]
                        edge nId [eId1, eId2, eId2]
                        return nId
                        

paramToDot :: Param -> Dot Id
paramToDot (ParExpr e) = toDot e
paramToDot (ParAbstr p e) = do
                             nId <- getFreshId
                             pId <- patToDot p
                             eId <- toDot e
                             node nId [Label $ SLabel "\\   ->", Color Blue, Shape Circle]
                             edge nId [pId, eId]
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
                        
recToDot (RecElem s e) = do
                          nId <- getFreshId
                          eId <- toDot e
                          node nId [Label $ SLabel s, Color Red, Shape Oval]
                          edge nId [eId]
                          return nId

keyToString :: Key -> String
keyToString (Key ks) = "(" ++ (concat $ L.intersperse ", " ks) ++ ")"