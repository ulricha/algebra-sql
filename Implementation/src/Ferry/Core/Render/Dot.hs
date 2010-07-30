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
toDot (Constant c) = let s = toString c
                      in node [Label $ SLabel s, Color Yellow, Shape Triangle]
toDot (Var i) = node [Label $ SLabel i, Color Red, Shape Triangle]
toDot (App c ps) = do
                     nId <- node [Label $ SLabel "$", Color Green, Shape Circle]
                     fId <- toDot c
                     pIds <- paramToDot ps
                     edge nId [fId, pIds]
                     return nId
toDot (Let s e1 e2) = do
                       nId <- node [Label $ SLabel "Let", Color Blue, Shape Rect]
                       id0 <- node [Label $ SLabel s, Color Red, Shape Rect, TextColor White]
                       id1 <- toDot e1
                       id2 <- toDot e2
                       edge nId [id0, id1, id2]
                       return nId
toDot (Rec es) = do
                  nId <- node [Label $ SLabel "Rec", Color Blue, Shape Oval]
                  eIds <- mapM recToDot es
                  edge nId eIds
                  return nId
toDot (Cons e1 e2) = do
                     nId <- node [Label $ SLabel "Cons", Color Blue, Shape Oval]
                     eIdh <- toDot e1
                     eIdt <- toDot e2
                     edge nId [eIdh, eIdt]
                     return nId
toDot (Nil)      = node [Label $ SLabel "Nil", Color Blue, Shape Oval]
toDot (Elem c s) = do
                    nId <- node [Label $ SLabel ".", Color Green, Shape Circle]
                    sId <- node [Label $ SLabel s, Color Red, Shape Triangle]
                    cId <- toDot c
                    edge nId [cId, sId]
                    return nId
toDot (Table n cs ks) = let label = VLabel $ ((HLabel [SLabel "Table:", SLabel n])
                                            : [HLabel [SLabel $ n ++ "::", SLabel $ prettyTy t ] | (Column n t) <- cs])
                                            ++ [SLabel $ keyToString k | k <- ks]
                         in node [Shape Rect, Label label, Color Yellow]
toDot (If e1 e2 e3) = do
                        nId <- node [Label $ SLabel "If", Color Blue, Shape Circle]
                        eId1 <- toDot e1
                        eId2 <- toDot e2
                        eId3 <- toDot e3
                        edge nId [eId1, eId2, eId2]
                        return nId
                        

paramToDot :: Param -> Dot Id
paramToDot (ParExpr e) = toDot e
paramToDot (ParAbstr p e) = do
                             nId <- node [Label $ SLabel "\\   ->", Color Blue, Shape Circle]
                             pId <- patToDot p
                             eId <- toDot e
                             edge nId [pId, eId]
                             return nId
                             
patToDot :: Pattern -> Dot Id
patToDot (PVar s) = node [Label $ SLabel s, Color Red, Shape Triangle]
patToDot (Pattern s) = node [Label $ SLabel $  "(" ++ (concat $ L.intersperse ", " s) ++ ")", Color Red, Shape Triangle]
recToDot (RecElem s e) = do
                          nId <- node [Label $ SLabel s, Color Red, Shape Oval]
                          eId <- toDot e
                          edge nId [eId]
                          return nId

keyToString :: Key -> String
keyToString (Key ks) = "(" ++ (concat $ L.intersperse ", " ks) ++ ")"