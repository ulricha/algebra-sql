{-# LANGUAGE TemplateHaskell #-}

module Database.Algebra.Rewrite.PatternConstruction
    ( dagPatMatch
    , v
    ) where

import           Control.Applicative
import           Control.Monad.Writer
import           Data.Maybe
import           Language.Haskell.TH

import           Database.Algebra.Dag
import           Database.Algebra.Dag.Common
import qualified Database.Algebra.Rewrite.DagRewrite    as R
import qualified Database.Algebra.Rewrite.Match         as M
import           Database.Algebra.Rewrite.PatternSyntax

type Code a = WriterT [Q Stmt] Q a

emit :: Q Stmt -> Code ()
emit s = tell [s]

matchOp :: Name
matchOp = mkName "matchOp"

opName :: Name
opName = mkName "op__internal"

terOpName :: Name
terOpName = mkName "TerOp"

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

data SemPattern = Bind (Q Pat, Name)
                | NoBind
                | NoSemantics

-- case op of ... -> return _ -> fail ""
instMatchCase :: Name           -- ^ The name of the node constructor (BinOp, UnOp, NullOp)
                 -> [Name]      -- ^ The name of the operator constructors
                 -> SemPattern -- ^ If the semantical pattern is not a wildcard: the name of the binding variable
                 -> [Q Pat]     -- ^ The list of patterns matching the node children
                 -> [Name]      -- ^ The list of variables for the children (may be empty)
                 -> Bool        -- ^ Bind the operator name (or don't)
                 -> Q Exp       -- ^ Returns the case expression
instMatchCase nodeConstructor opConstructors semantics childMatchPatterns childNames bindOp =
  caseE (varE opName) ((map opAlternative opConstructors) ++ [catchAllCase])
  where opAlternative opConstructor = match opPattern opBody []
          where (semPat, semName) = case semantics of
                  Bind (p, n) -> ([p], [n])
                  NoBind      -> ([wildP], [])
                  NoSemantics -> ([], [])
                opPattern = conP nodeConstructor ((conP opConstructor semPat) : childMatchPatterns)
                opConstExp = if bindOp then [conE opConstructor] else []
                opBody = normalB $ appE (varE (mkName "return")) (tupE $ opConstExp ++ (map varE (semName ++ childNames)))

-- \op -> case op of...
instMatchLambda :: Q Exp -> Q Exp
instMatchLambda body = lam1E (varP opName) body

instMatchExp :: Name -> Q Exp -> Q Exp
instMatchExp nodeName matchLambda =
  appE (appE (varE matchOp) (varE nodeName)) matchLambda

-- (a, b, c) <- ...
instBindingPattern :: Maybe (Q Pat) -> SemPattern -> [Q Pat] -> Q Pat
instBindingPattern mOpConstPat semPat childPats = tupP patterns
  where patterns = (maybeList mOpConstPat) ++ (semList semPat) ++ childPats
        maybeList (Just x) = [x]
        maybeList Nothing  = []

        semList (Bind (pat, _)) = [pat]
        semList NoBind         = []
        semList NoSemantics    = []

-- (a, b, c) <- matchOp q (\op -> case op of ...)
instStatement :: Maybe (Q Pat) -> SemPattern -> [Q Pat] -> Q Exp -> Q Stmt
instStatement mOpConstPat semPat childPats matchExp =
  case (semPat, childPats) of
    (NoBind, [])      -> noBindS matchExp
    (NoSemantics, []) -> noBindS matchExp
    (_, _)            -> bindS (instBindingPattern mOpConstPat semPat childPats) matchExp

semPatternName :: Maybe Sem -> SemPattern
semPatternName (Just WildS)      = NoBind
semPatternName (Just (NamedS s)) = let name = mkName s in Bind (varP name, name)
semPatternName Nothing           = NoSemantics

instStmtWrapper :: Name              -- ^ The name of the node on which to match
                   -> Name           -- ^ The name of the node constructor (BinOp, UnOp, NullOp)
                   -> [Name]         -- ^ The name of the operator constructors
                   -> Maybe (Q Pat)  -- ^ The binding name for the operator constructor
                   -> SemPattern     -- ^ Pattern binding the semantical information (or wildcard)
                   -> [Q Pat]        -- ^ The list of patterns matching the node children
                   -> [Q Pat]        -- ^ The list of patterns binding the node children (may be empty)
                   -> [Name]         -- ^ The list of variables for the children (may be empty)
                   -> Q Stmt         -- ^ Returns the case expression
instStmtWrapper nodeName nodeKind operNames mOpConstPat semantics childMatchPats childPats childNames =
  let matchCase   = instMatchCase nodeKind operNames semantics childMatchPats childNames (isJust mOpConstPat)
      matchLambda = instMatchLambda matchCase
      matchExp    = instMatchExp nodeName matchLambda
  in instStatement mOpConstPat semantics childPats matchExp

opInfo :: Op -> (Maybe (Q Pat), [Name])
opInfo (NamedOp bindingName opNames) = (Just $ varP $ mkName bindingName, map mkName opNames)
opInfo (UnnamedOp opNames) = (Nothing, map mkName opNames)

-- generate a list of node matching statements from an operator (tree)
gen :: Name -> Node -> Code ()
gen nodeName (NullP op semBinding) =
  let semantics = semPatternName semBinding
      (mOpConstPat, opNames) = opInfo op
      statement = instStmtWrapper nodeName nullOpName opNames mOpConstPat semantics [] [] []
  in emit statement

gen nodeName (UnP op semBinding child) = do
  let semantics = semPatternName semBinding
      (mOpConstPat, opNames) = opInfo op

  patAndName <- lift (childMatchPattern child)

  let (matchPatterns, bindNames, bindPatterns) = splitMatchAndBind $ [patAndName]
      statement = instStmtWrapper nodeName unOpName opNames mOpConstPat semantics matchPatterns bindPatterns bindNames

  emit statement

  maybeDescend child (snd patAndName)

gen nodeName (BinP op semBinding child1 child2) = do
  let semantics = semPatternName semBinding
      (mOpConstPat, opNames) = opInfo op

  leftPatAndName   <- lift (childMatchPattern child1)
  rightPatAndName  <- lift (childMatchPattern child2)

  let (matchPatterns, bindNames, bindPatterns) = splitMatchAndBind [leftPatAndName, rightPatAndName]
      statement = instStmtWrapper nodeName binOpName opNames mOpConstPat semantics matchPatterns bindPatterns bindNames

  emit statement

  maybeDescend child1 (snd leftPatAndName)
  maybeDescend child2 (snd rightPatAndName)

gen nodeName (TerP op semBinding child1 child2 child3) = do
  let semantics = semPatternName semBinding
      (mOpConstPat, opNames) = opInfo op

  patAndName1 <- lift (childMatchPattern child1)
  patAndName2 <- lift (childMatchPattern child2)
  patAndName3 <- lift (childMatchPattern child3)

  let childPatAndNames = [patAndName1, patAndName2, patAndName3]
      (matchPatterns, bindNames, bindPatterns) = splitMatchAndBind $ childPatAndNames
      statement = instStmtWrapper nodeName terOpName opNames mOpConstPat semantics matchPatterns bindPatterns bindNames

  emit statement

  maybeDescend child1 (snd patAndName1)
  maybeDescend child2 (snd patAndName2)
  maybeDescend child3 (snd patAndName3)

gen nodeName (HoleP holeStart subHolePat) = do
  -- collect all binders from the sub-hole pattern in a canonical order
  let binderNames = map mkName $ collectBinders subHolePat

  -- generate a function that tries to match the sub-hole pattern at the given node
  (patMatchFunName, patMatchFunStmt) <- lift $ genSubHoleMatch binderNames subHolePat
  emit patMatchFunStmt

  -- (nodeName, binderNames) <- searchHolePat patMatchName holeStart
  -- Use function searchHolePat to search for a node at which the sub-hole
  -- pattern matches.
  let searchExpr = appE (appE (varE 'searchHolePat) (varE patMatchFunName)) (varE nodeName)
      bindingPat = tupP [varP (mkName holeStart), listP (map varP binderNames)]

  emit $ bindS bindingPat searchExpr

gen nodeName (HoleEq eqNode) = do
  emit $ noBindS $ appE (appE (varE 'searchHoleEq) (varE nodeName)) (varE $ mkName eqNode)

-- Traverse a DAG (DFS, preorder) and search for a node where the given pattern applies.
-- Returns the matching node and the list of values for the pattern's binders.
searchHolePat :: Operator o
                 => (AlgNode -> M.Match o p e [AlgNode])
                 -> AlgNode
                 -> M.Match o p e (AlgNode, [AlgNode])
searchHolePat patMatch q = do
  (d, p, e) <- M.exposeEnv
  case M.runMatch e d p (patMatch q) of
    Just nodes -> return (q, nodes)
    Nothing    -> do
                    children <- opChildren <$> M.getOperator q
                    searchChildren patMatch children

-- Apply searchHolePat to a list of nodes, take the first one that matches.
searchChildren :: Operator o
                  => (AlgNode -> M.Match o p e [AlgNode])
                  -> [AlgNode]
                  -> M.Match o p e (AlgNode, [AlgNode])
searchChildren _        []     = fail "no match"
searchChildren patMatch (q:qs) = do
  (d, p, e) <- M.exposeEnv
  case M.runMatch e d p(searchHolePat patMatch q) of
    Just nodes -> return nodes
    Nothing    -> searchChildren patMatch qs

-- Search for an occurence of the node 'eqNode', starting at 'startNode'
searchHoleEq :: Operator o => AlgNode -> AlgNode -> M.Match o p e ()
searchHoleEq startNode eqNode =
  if startNode == eqNode
  then return ()
  else do
    (d, _, _) <- M.exposeEnv
    children <- opChildren <$> M.getOperator startNode
    if nodeOccurs d eqNode children
      then return ()
      else fail "no occurence"

-- Since we only search for occurences of a particular node and no pattern matching
-- occurs, we do not burden ourselves with the Match monad here.
nodeOccurs :: Operator o => AlgebraDag o -> AlgNode -> [AlgNode] -> Bool
nodeOccurs dag eqNode startNodes =
  if eqNode `elem` startNodes
  then True
  else or $ map (nodeOccurs dag eqNode . opChildren . (flip operator dag)) startNodes

-- | Generate a function which matches a pattern on a certain node.
-- The generated function returns values for all binders in the pattern
-- in the canonical order given by 'binderNames'
-- Type of the generated function:
--      subhole_xy :: AlgNode -> Match o [AlgNode]
genSubHoleMatch :: [Name] -> Pattern -> Q (Name, Q Stmt)
genSubHoleMatch binderNames pat = do
  -- generate the code for matching the pattern
  rootName <- newName "subNode"
  patternStatements <- execWriterT $ gen rootName pat
  -- return values for the binders in the proper order.
  let returnStatement   = noBindS $ appE (varE 'return) (listE $ map varE binderNames)
      body              = doE $ patternStatements ++ [returnStatement]

  -- the function binding
  funName <- newName "subhole"
  let fun  = funD funName [(clause [varP rootName] (normalB body) [])]
      stmt = letS $ [fun]
  return (funName, stmt)

{-
semBinder :: Maybe Sem -> [Ident]
semBinder (Just (NamedS i)) = [i]
semBinder (Just WildS)      = []
semBinder Nothing           = []
-}

opBinder :: Op -> [Ident]
opBinder (NamedOp i _) = [i]
opBinder (UnnamedOp _) = []

childBinders :: Child -> [Ident]
childBinders (NodeC n)        = collectBinders n
childBinders WildC            = []
childBinders (NameC i)        = [i]
childBinders (NamedNodeC i n) = i : collectBinders n

-- Collect binders in pre-order fashion from a pattern tree
-- TODO: so far, only binders for nodes (type AlgNode) are collected. This is
-- necessary so that we can return values for them in a list without type-specific
-- wrappers
collectBinders :: Node -> [Ident]
collectBinders (TerP op _ c1 c2 c3) = opBinder op
                                      -- ++ semBinder sem
                                      ++ concatMap childBinders [c1, c2, c3]
collectBinders (BinP op _ c1 c2)    = opBinder op
                                      -- ++ semBinder sem
                                      ++ concatMap childBinders [c1, c2]
collectBinders (UnP op _ c)         = opBinder op
                                      -- ++ semBinder sem
                                      ++ childBinders c
collectBinders (NullP op _)         = opBinder op -- ++ semBinder sem
collectBinders (HoleP _ _)          = error "collectBinders: Holes in sub-hole patterns not supported"
collectBinders (HoleEq _)           = []

{-
Split the list of matching patterns and binding names.
-}
splitMatchAndBind :: [(Q Pat, Maybe Name)] -> ([Q Pat], [Name], [Q Pat])
splitMatchAndBind ps =
  let (matchPatterns, mBindNames) = unzip ps
      bindNames = catMaybes mBindNames
  in (matchPatterns, bindNames, map varP bindNames)

{-
For every child, generate the matching pattern and - if the child
is to be bound either with a given name or for matching on the child itself -
the name to which it should be bound.

This distinction is necessary because a child that is not to be bound
must be matched anyway with a wildcard pattern so that the operator constructor
has enough parameters in the match.
-}
childMatchPattern :: Child -> Q (Q Pat, Maybe Name)
childMatchPattern WildC   =
  return (wildP, Nothing)
childMatchPattern (NameC s) =
  let n = mkName s
  in return (varP n, Just n)
childMatchPattern (NodeC _) =
  newName "child"
  >>= (\n -> return (varP n, Just n))
childMatchPattern (NamedNodeC s _) =
  let n = mkName s
  in return (varP n, Just n)

recurse :: Child -> Maybe Node
recurse WildC           = Nothing
recurse (NameC _)       = Nothing
recurse (NodeC o)         = Just o
recurse (NamedNodeC _ o ) = Just o

maybeDescend :: Child -> Maybe Name -> Code ()
maybeDescend c ns =
  case recurse c of
    Just o   ->
      case ns of
        Just n   -> gen n o
        Nothing  -> error "PatternConstruction.gen: no name for child pattern"
    Nothing  -> return ()

assembleStatements :: Q [Stmt] -> Q Exp -> Q Exp
assembleStatements patternStatements userExpr = do
  ps <- patternStatements
  e <- userExpr

  let us =
        case e of
          DoE userStatements -> userStatements
          _ -> error "PatternConstruction.assembleStatements: no do-block supplied"

      -- The call to collect
      collectStmt = NoBindS $ VarE 'R.collect

      -- Extract the returned sequence of rewrite actions and patch the
      -- call to collect at the end
      returnStmt = case last us of
                     NoBindS (InfixE (Just (VarE returnName)) (VarE dollarName) (Just rewriteExpr))
                       | dollarName == '($) && returnName == 'return    ->
                         let rewriteExpr' = DoE [NoBindS rewriteExpr, collectStmt]
                         in NoBindS (InfixE (Just (VarE returnName)) (VarE dollarName) (Just rewriteExpr'))
                     s                                                  -> error $ show s

      -- reassemble the user statements
      us' = init us ++ [returnStmt]

  -- Return a do block consisting of the pattern statements and the user statements.
  return $ DoE $ ps ++ us'

-- | Take a quoted variable with the root node on which to apply the pattern,
-- a string description of the pattern and the body of the match
-- and return the complete match statement. The body has to be a quoted ([| ...|])
-- do-block.
dagPatMatch :: Name -> String -> Q Exp -> Q Exp
dagPatMatch rootName patternString userExpr = do

  let pat = parsePattern patternString

  -- generate the code that matches the pattern (a list of statements)
  patternStatements <- execWriterT $ gen rootName pat

  -- combine the generated pattern-matching statements with the
  -- user-supplied additional predicates
  assembleStatements (mapM id patternStatements) userExpr

-- | Reference a variable that is bound by a pattern in a quoted match body.
v :: String -> Q Exp
v = dyn
