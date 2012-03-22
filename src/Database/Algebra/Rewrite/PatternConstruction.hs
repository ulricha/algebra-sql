{-# LANGUAGE TemplateHaskell #-}

module Database.Algebra.Rewrite.PatternConstruction( pattern, v ) where

import Language.Haskell.TH
import Control.Monad.Writer
import Data.Maybe
  
import Database.Algebra.Rewrite.PatternSyntax

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
                 -> [Name]      -- ^ The name of the operator constructors
                 -> Maybe (Q Pat, Name) -- ^ If the semantical pattern is not a wildcard: the name of the binding variable
                 -> [Q Pat]     -- ^ The list of patterns binding the node children
                 -> [Name]      -- ^ The list of variables for the children (may be empty)
                 -> Bool        -- ^ Bind the operator name
                 -> Q Exp       -- ^ Returns the case expression
instMatchCase nodeConstructor opConstructors semantics childPatterns childNames bindOp = 
  caseE (varE opName) ((map opAlternative opConstructors) ++ [catchAllCase])
  where opAlternative opConstructor = match opPattern opBody []
          where (semPat, semName) = case semantics of
                  Just (p, n) -> (p, [n])
                  Nothing     -> (wildP, [])
                opPattern = conP nodeConstructor ((conP opConstructor [semPat]) : childPatterns)
                opConstExp = if bindOp then [conE opConstructor] else []
                opBody = normalB $ appE (varE (mkName "return")) (tupE $ opConstExp ++ (map varE (semName ++ childNames)))
        
-- \op -> case op of...
instMatchLambda :: Q Exp -> Q Exp
instMatchLambda body = lam1E (varP opName) body
                       
instMatchExp :: Name -> Q Exp -> Q Exp
instMatchExp nodeName matchLambda = 
  appE (appE (varE matchOp) (varE nodeName)) matchLambda
  
                       
-- (a, b, c) <- ...
instBindingPattern :: Maybe (Q Pat) -> Maybe (Q Pat) -> [Q Pat] -> Q Pat
instBindingPattern mOpConstPat mSemPat childPats = tupP patterns
  where patterns = (maybeList mOpConstPat) ++ (maybeList mSemPat) ++ childPats
        maybeList (Just x) = [x]
        maybeList Nothing  = []
  
-- (a, b, c) <- matchOp q (\op -> case op of ...)
instStatement :: Maybe (Q Pat) -> Maybe (Q Pat) -> [Q Pat] -> Q Exp -> Q Stmt
instStatement mOpConstPat mSemPat childPats matchExp =
  case (mSemPat, childPats) of
    (Nothing, []) -> noBindS matchExp
    (_, _)        -> bindS (instBindingPattern mOpConstPat mSemPat childPats) matchExp
  
semPatternName :: Sem -> Maybe (Q Pat, Name)
semPatternName WildS      = Nothing
semPatternName (NamedS s) = let name = mkName s in Just (varP name, name)
                                                   
instStmtWrapper :: Name              -- ^ The name of the node on which to match
                   -> Name           -- ^ The name of the node constructor (BinOp, UnOp, NullOp)
                   -> [Name]         -- ^ The name of the operator constructors
                   -> Maybe (Q Pat)  -- ^ The binding name for the operator constructor
                   -> Maybe (Q Pat, Name)  -- ^ Pattern binding the semantical information (or wildcard)
                   -> [Q Pat]        -- ^ The list of patterns binding the node children
                   -> [Name]         -- ^ The list of variables for the children (may be empty)
                   -> Q Stmt         -- ^ Returns the case expression
instStmtWrapper nodeName nodeKind operNames mOpConstPat semantics childPats childNames =
  let matchCase   = instMatchCase nodeKind operNames semantics childPats childNames (isJust mOpConstPat)
      matchLambda = instMatchLambda matchCase
      matchExp    = instMatchExp nodeName matchLambda
  in instStatement mOpConstPat (fmap fst semantics) childPats matchExp
     
opInfo :: Op -> (Maybe (Q Pat), [Name])
opInfo (NamedOp bindingName opNames) = (Just $ varP $ mkName bindingName, map mkName opNames)
opInfo (UnnamedOp opNames) = (Nothing, map mkName opNames)
       
-- generate a list of statements from an operator (tree)
gen :: Name -> Node -> Code ()
gen nodeName (NullP op semBinding) = 
  let semantics = semPatternName semBinding
      (mOpConstPat, opNames) = opInfo op
      statement = instStmtWrapper nodeName nullOpName opNames mOpConstPat semantics [] []
  in emit statement
     
gen nodeName (UnP op semBinding child) = do
  let semantics = semPatternName semBinding 
      (mOpConstPat, opNames) = opInfo op
      
  name <- lift (childName child)
  
  let childPattern = map varP name
      statement = instStmtWrapper nodeName unOpName opNames mOpConstPat semantics childPattern name
  
  emit statement
  
  maybeDescend child name
                  
gen nodeName (BinP op semBinding child1 child2) = do
  let semantics = semPatternName semBinding
      (mOpConstPat, opNames) = opInfo op
  
  nameLeft   <- lift (childName child1)
  nameRight  <- lift (childName child2)
  
  let childNames = nameLeft ++ nameRight
      childPatterns = map varP childNames
      statement = instStmtWrapper nodeName binOpName opNames mOpConstPat semantics childPatterns childNames
      
  emit statement
  
  maybeDescend child1 nameLeft
  maybeDescend child2 nameRight
  
childName :: Child -> Q [Name]
childName WildC          = return []
childName (NameC s)      = return [mkName s]
childName (NodeC _)        = newName "child" >>= (\n -> return [n])
childName (NamedNodeC s _) = return [mkName s]
                             
recurse :: Child -> Maybe Node
recurse WildC           = Nothing
recurse (NameC _)       = Nothing
recurse (NodeC o)         = Just o
recurse (NamedNodeC _ o ) = Just o
                         
maybeDescend :: Child -> [Name] -> Code ()                         
maybeDescend c ns =
  case recurse c of
    Just o   ->  
      case ns of
        [n] -> gen n o
        _   -> error "PatternConstruction.gen: no name for child pattern"
    Nothing  -> return ()
                                                        

assembleStatements :: Q [Stmt] -> Q Exp -> Q Exp
assembleStatements patternStatements userExpr = do
  ps <- patternStatements
  e <- userExpr
  
  let us = 
        case e of
          DoE userStatements -> userStatements
          _ -> error "PatternConstruction.assembleStatements: no do-block supplied"
          
  return $ DoE $ ps ++ us
  
-- | Take q quoted variable with the root node on which to apply the pattern, 
-- a string description of the pattern and the body of the match
-- and return the complete match statement. The body has to be a quoted ([| ...|])
-- do-block.
pattern :: Q Exp -> String -> Q Exp -> Q Exp
pattern rootExp patternString userExpr = do
  
  re <- rootExp
  
  let rootName = case re of 
        VarE n -> n
        _      -> error "supplied root node is not a quoted variable"
  
  let pat = parsePattern patternString
  
  patternStatements <- execWriterT $ gen rootName pat
  
  assembleStatements (mapM id patternStatements) userExpr
                          
-- | Reference a variable that is bound by a pattern in a quoted match body.
v :: String -> Q Exp 
v = dyn
