{-# LANGUAGE TemplateHaskell #-}

module Database.Algebra.Rewrite.PatternConstruction where

import Language.Haskell.TH
import Control.Monad.Writer
  
import Database.Algebra.Rewrite.PatternSyntax

-- Take a string description of the pattern and the body of the match
-- and return the complete match statement.
m :: String -> Q Exp -> Q Exp
m = undefined
    
type Code a = WriterT [Q Stmt] Q a

emit :: Q Stmt -> Code ()
emit s = tell [s]
       
matchOp :: Name
matchOp = mkName "matchOp"
          
opName :: Name
opName = mkName "op"

binOpName :: Name
binOpName = mkName "BinOp"

unOpName :: Name
unOpName = mkName "UnOp"

nullOpName :: Name
nullOpName = mkName "NullOp"
         
failName :: Name
failName = mkName "fail"
         
catchAllCase :: Q Match
catchAllCase = match wildP (normalB (appE (varE failName) (litE (stringL "")))) []
         
-- case op of ... -> return _ -> fail ""
instMatchCase :: Name           -- ^ The name of the node constructor (BinOp, UnOp, NullOp)
                 -> Name        -- ^ The name of the operator constructor
                 -> Q Pat       -- ^ Pattern binding the semantical information (or wildcard)
                 -> Maybe Name  -- ^ If the semantical pattern is not a wildcard: the name of the binding variable
                 -> [Q Pat]     -- ^ The list of patterns binding the node children
                 -> [Name]      -- ^ The list of variables for the children (may be empty)
                 -> Q Exp       -- ^ Returns the case expression
instMatchCase nodeConstructor opConstructor semPattern mSemName childPatterns childNames = 
  caseE (varE opName) [opCase, catchAllCase]
  where opCase = match opPattern opBody []
        opPattern = conP nodeConstructor ((conP opConstructor [semPattern]) : childPatterns)
        bodyNames = case mSemName of
                     Just n  -> n : childNames
                     Nothing -> childNames
        opBody = normalB $ appE (varE (mkName "return")) (tupE $ map varE bodyNames)
        
-- \op -> case op of...
instMatchLambda :: Q Exp -> Q Exp
instMatchLambda body = lam1E (varP opName) body
                       
instMatchExp :: Name -> Q Exp -> Q Exp
instMatchExp nodeName matchLambda = 
  appE (appE (varE matchOp) (varE nodeName)) matchLambda
  
                       
-- (a, b, c) <- ...
instBindingPattern :: Maybe (Q Pat) -> [Q Pat] -> Q Pat
instBindingPattern mSemPat childPats =
  case mSemPat of
    Just semPat -> tupP $ semPat : childPats
    Nothing     -> tupP childPats
  
-- (a, b, c) <- matchOp q (\op -> case op of ...)
instStatement :: Maybe (Q Pat) -> [Q Pat] -> Q Exp -> Q Stmt
instStatement mSemPat childPats matchExp =
  case (mSemPat, childPats) of
    (Nothing, []) -> noBindS matchExp
    (_, _)        -> bindS (instBindingPattern mSemPat childPats) matchExp
  
semPatternName :: Sem -> (Q Pat, Maybe Name)
semPatternName WildS      = (wildP, Nothing)
semPatternName (NamedS s) = let name = mkName s in (varP name, Just name)
       
-- generate a list of statements from an operator (tree)
gen :: Name -> Op -> Code ()
gen nodeName (NullP opName semBinding) = 
  let (semPat, mSemName) = semPatternName semBinding
      matchCase     = instMatchCase nullOpName (mkName opName) semPat mSemName [] []
      matchLambda   = instMatchLambda matchCase
      matchExp      = instMatchExp nodeName matchLambda
      statement     = if semBinding == WildS 
                      then instStatement Nothing [] matchExp 
                      else instStatement (Just semPat) [] matchExp
  in emit statement
gen nodeName (UnP opName semBinding child) = 
  let (semPat, mSemName) = semPatternName semBinding
      matchCase     = instMatchCase nullOpName (mkName opName) semPat mSemName [] []
      matchLambda   = instMatchLambda matchCase
      matchExp      = instMatchExp nodeName matchLambda
      statement     = if semBinding == WildS 
                      then instStatement Nothing [] matchExp 
                      else instStatement (Just semPat) [] matchExp
  in do 
    emit statement
                              
bindingTuple :: [String] -> Q Pat
bindingTuple ss = tupP $ map (varP . mkName) ss
  
