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
                 -> Maybe (Q Pat, Name) -- ^ If the semantical pattern is not a wildcard: the name of the binding variable
                 -> [Q Pat]     -- ^ The list of patterns binding the node children
                 -> [Name]      -- ^ The list of variables for the children (may be empty)
                 -> Q Exp       -- ^ Returns the case expression
instMatchCase nodeConstructor opConstructor semantics childPatterns childNames = 
  caseE (varE opName) [opCase, catchAllCase]
  where opCase = match opPattern opBody []
        (semPat, semName) = case semantics of
                              Just (p, n) -> (p, [n])
                              Nothing     -> (wildP, [])
        opPattern = conP nodeConstructor ((conP opConstructor [semPat]) : childPatterns)
        opBody = normalB $ appE (varE (mkName "return")) (tupE $ map varE (semName ++ childNames))
        
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
  
semPatternName :: Sem -> Maybe (Q Pat, Name)
semPatternName WildS      = Nothing
semPatternName (NamedS s) = let name = mkName s in Just (varP name, name)
                                                   
instStmtWrapper :: Name              -- ^ The name of the node on which to match
                   -> Name           -- ^ The name of the node constructor (BinOp, UnOp, NullOp)
                   -> Name           -- ^ The name of the operator constructor
                   -> Maybe (Q Pat, Name)  -- ^ Pattern binding the semantical information (or wildcard)
                   -> [Q Pat]        -- ^ The list of patterns binding the node children
                   -> [Name]         -- ^ The list of variables for the children (may be empty)
                   -> Q Stmt         -- ^ Returns the case expression
instStmtWrapper nodeName nodeKind operName semantics childPats childNames =
  let matchCase   = instMatchCase nodeKind operName semantics childPats childNames
      matchLambda = instMatchLambda matchCase
      matchExp    = instMatchExp nodeName matchLambda
  in instStatement (fmap fst semantics) childPats matchExp
       
-- generate a list of statements from an operator (tree)
gen :: Name -> Op -> Code ()
gen nodeName (NullP opString semBinding) = 
  let semantics = semPatternName semBinding
      statement = instStmtWrapper nodeName nullOpName (mkName opString) semantics [] []
  in emit statement
     
gen nodeName (UnP opString semBinding child) = do
  let semantics = semPatternName semBinding 
      
  name <- lift (childName child)
  
  let childPattern = map varP name
      statement = instStmtWrapper nodeName unOpName (mkName opString) semantics childPattern name
  
  emit statement
  
  maybeDescend child name
                  
gen nodeName (BinP opString semBinding child1 child2) = do
  let semantics = semPatternName semBinding
  
  nameLeft   <- lift (childName child1)
  nameRight  <- lift (childName child2)
  
  let childNames = nameLeft ++ nameRight
      childPatterns = map varP childNames
      statement = instStmtWrapper nodeName binOpName (mkName opString) semantics childPatterns childNames
      
  emit statement
  
  maybeDescend child1 nameLeft
  maybeDescend child2 nameRight
  
childName :: Child -> Q [Name]
childName WildC          = return []
childName (NameC s)      = return [mkName s]
childName (OpC _)        = newName "child" >>= (\n -> return [n])
childName (NamedOpC s _) = return [mkName s]
                             
recurse :: Child -> Maybe Op
recurse WildC           = Nothing
recurse (NameC _)       = Nothing
recurse (OpC o)         = Just o
recurse (NamedOpC _ o ) = Just o
                         
maybeDescend :: Child -> [Name] -> Code ()                         
maybeDescend c ns =
  case recurse c of
    Just o   ->  
      case ns of
        [n] -> gen n o
        _   -> error "PatternConstruction.gen: no name for child pattern"
    Nothing  -> return ()
                                                        

bindingTuple :: [String] -> Q Pat
bindingTuple ss = tupP $ map (varP . mkName) ss
  
