import qualified Data.IntMap as IntMap
    ( fromList
    )
import Control.Monad (when, forM_)
import System.Console.GetOpt
    ( getOpt
    , ArgOrder(RequireOrder)
    , usageInfo
    , ArgDescr(NoArg, ReqArg, OptArg)
    , OptDescr(Option)
    )
import System.Environment (getArgs)

import qualified Database.Algebra.Pathfinder.Data.Algebra as A
import qualified Database.Algebra.Dag as D
import qualified Database.Algebra.Dag.Common as C

import Database.Algebra.SQL.File
import Database.Algebra.SQL.Materialization
import Database.Algebra.SQL.Materialization.CTE as CTE
import Database.Algebra.SQL.Materialization.TemporaryTable as TemporaryTable
import qualified Database.Algebra.SQL.Materialization.Combined as Combined
import Database.Algebra.SQL.Util
    ( renderDebugOutput
    , renderOutput
    , putShowSLn
    , renderAdvancedDebugOutput
    )
import qualified Database.Algebra.SQL.Tile as T



test :: T.PFDag
test = D.mkDag ( IntMap.fromList [ (0, C.BinOp (A.Cross ()) 1 2)
                                 , (1, C.UnOp (A.Project [("x", A.ColE "y")]) 4)
                                 , (2, C.BinOp (A.Cross ()) 1 3)
                                 , (4, C.NullaryOp $ A.LitTable
                                                     [[A.VStr "foo"]]
                                                     [("y", A.AStr)]
                                   )
                                 , (3, C.UnOp (A.Project [("z", A.ColE "y")]) 4)
                                 , (5, C.UnOp (A.Project [("x", A.ColE "y")]) 4)
                                 ]
               )
               [0, 5]

-- TODO when using mutiple root nodes the result gets computed multiple times,
-- maybe save every computed tile in the state part of the transform monad for a node.
g1 :: T.PFDag
g1 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Project [("result", A.ColE "str")]) 1)
                               , (1, C.UnOp (A.Select eq) 2)
                               , (2, C.NullaryOp $ A.LitTable
                                                   [ [A.VStr "0"]
                                                   , [A.VStr "1"]
                                                   , [A.VStr "2"]
                                                   ]
                                                   [("str", A.AStr)]
                                 )
                               ]
                               
             )
             [0]
   where eq   = A.BinAppE A.Eq (A.BinAppE A.Plus cast $ A.ConstE $ A.VInt 41)
                               $ A.ConstE $ A.VInt 42
         cast = A.UnAppE (A.Cast A.AInt) $ A.ColE "str"

g2 :: T.PFDag
g2 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Project [ ( "result"
                                                         , A.BinAppE A.Plus
                                                                     (A.ColE "a")
                                                                     $ A.ColE "b"
                                                         )
                                                       ]) 2)
                               , (2, C.BinOp (A.EqJoin ("u", "v")) 3 4)
                               , (3, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 0, A.VInt 50]
                                                   , [A.VInt 1, A.VInt 60]
                                                   ]
                                                   [("u", A.AInt), ("a", A.AInt)]
                                                 )
                                 )
                               , (4, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 0, A.VInt 4]
                                                   , [A.VInt 1, A.VInt 23]
                                                   ]
                                                   [("v", A.AInt), ("b", A.AInt)]
                                                 )
                                 )
                               ]
             )
             [0]

--g3 :: T.PFDag
--g3 = D.mkDag ( IntMap.fromList [ (0, C.BinOp (A.EqJoin ("a", "b")) 1 2)
--                               , (1, C.UnOp (A.Proj [("a", "u")]) 3)
--                               , (2, C.UnOp (A.Proj [("b", "u")]) 3)
--                               , (3, C.NullaryOp ( A.LitTable
--                                                   [ [A.VInt 0]
--                                                   ]
--                                                   [("u", A.AInt)]
--                                                 )
--                                 )
--                               ]
--             )
--             [0, 1, 2]
--
--g4 :: T.PFDag
--g4 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Proj [("qsum", "qsum"), ("id", "id")]) 1)
--                               , (1, C.UnOp (A.Aggr ([(A.Sum "quantity", "qsum")], Just "id")) 2) 
--                               , (2, C.NullaryOp ( A.LitTable
--                                                   [ [A.VInt 0, A.VInt 2]
--                                                   , [A.VInt 0, A.VInt 10]
--                                                   , [A.VInt 1, A.VInt 100]
--                                                   , [A.VInt 1, A.VInt 20]
--                                                   ]
--                                                   [("id", A.AInt), ("quantity", A.AInt)]
--                                                 )
--                                 )
--                               ]
--             )
--             [0]
--
--g5 :: T.PFDag
--g5 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Proj [("y", "id")]) 1)
--                               , (1, C.UnOp (A.Proj [("y", "id")]) 3)
--                               , (2, C.UnOp (A.Proj [("y", "id")]) 3)
--                               , (3, C.NullaryOp ( A.TableRef
--                                                   ( "foo"
--                                                   , [ ("id", "id",  A.AInt)
--                                                     , ("bla", "bla",  A.AStr)
--                                                     ]
--                                                   , []
--                                                   )
--                                                 )
--                                 )
--                               ]
--             )
--             [0, 1]
--

g6 :: T.PFDag
g6 = D.mkDag ( IntMap.fromList [ (0, C.BinOp ( A.AntiJoin [ ("a", "c", A.EqJ)
                                                          , ("b", "d", A.LtJ)
                                                          ]
                                             )
                                             1
                                             2
                                 )
                               , (1, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 1, A.VInt 2]
                                                   , [A.VInt 1, A.VInt 1]
                                                   ]
                                                   [("a", A.AInt), ("b", A.AInt)]
                                                 )
                                 )
                               , (2, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 1, A.VInt 2]
                                                   , [A.VInt 1, A.VInt 1]
                                                   ]
                                                   [("c", A.AInt), ("d", A.AInt)]
                                                 )
                                 )
                               ]
             )
             [0]

g7 :: T.PFDag
g7 = D.mkDag ( IntMap.fromList [ (0, C.BinOp ( A.SemiJoin [ ("a", "c", A.EqJ)
                                                          , ("b", "d", A.LtJ)
                                                          ]
                                             )
                                             1
                                             2
                                 )
                               , (1, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 1, A.VInt 2]
                                                   , [A.VInt 1, A.VInt 1]
                                                   ]
                                                   [("a", A.AInt), ("b", A.AInt)]
                                                 )
                                 )
                               , (2, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 1, A.VInt 2]
                                                   , [A.VInt 1, A.VInt 1]
                                                   ]
                                                   [("c", A.AInt), ("d", A.AInt)]
                                                 )
                                 )
                               ]
             )
             [0]

-- Should use EXISTS due to the lack of the equal join condition.
g8 :: T.PFDag
g8 = D.mkDag ( IntMap.fromList [ (0, C.BinOp ( A.SemiJoin [ ("b", "d", A.LtJ)
                                                          ]
                                             )
                                             1
                                             2
                                 )
                               , (1, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 1, A.VInt 2]
                                                   , [A.VInt 1, A.VInt 1]
                                                   ]
                                                   [("a", A.AInt), ("b", A.AInt)]
                                                 )
                                 )
                               , (2, C.NullaryOp ( A.LitTable
                                                   [ [A.VInt 1, A.VInt 2]
                                                   , [A.VInt 1, A.VInt 1]
                                                   ]
                                                   [("c", A.AInt), ("d", A.AInt)]
                                                 )
                                 )
                               ]
             )
             [0]

g9 :: T.PFDag
g9 = D.mkDag ( IntMap.fromList [ (0, C.UnOp p 2)
                               , (1, C.UnOp p 2)
                               , (2, C.UnOp p 4)
                               , (3, C.UnOp p 4)
                               , (4, C.UnOp p 5)
                               , (5, C.BinOp eq 6 7)
                               , (6, C.UnOp (pc "a") 8)
                               , (7, C.UnOp p 8)
                               , (8, C.UnOp p 9)
                               , (9, C.NullaryOp ( A.LitTable [] [("c", A.AInt), ("d", A.AInt)] )
                                 )
                               ]
             )
             [0, 1, 3]
  where p    = pc "c"
        pc c = A.Project [(c, A.ColE "c")]
        eq   = A.EqJoin ("a", "c")

-- | Tests whether different binding strategies for Materialization.Combined
-- work.
g10 :: T.PFDag
g10 = D.mkDag ( IntMap.fromList [ (0, C.BinOp eq 1 1)
                                , (1, C.BinOp eq 2 2)
                                , (2, C.UnOp p 3)
                                , (3, C.NullaryOp ( A.LitTable [] [("c", A.AInt), ("d", A.AInt)] ))
                                ]
              )
              [0]
  where p    = pc "c"
        pc c = A.Project [(c, A.ColE "c")]
        eq   = A.EqJoin ("c", "d")


testGraphs :: [T.PFDag]
testGraphs = singleTests ++ [test, g1, g2, g6, g7, g8, g9, g10] -- g3, g4, g5]

-- Test for single operator translation.
singleTests :: [T.PFDag]
singleTests = [ tLitTable
              , tEmptyTable
              , tTableRef
              , tRowNum
              , tRowRank
              , tRank
              , tProject
              , tSelect
              , tDistinct
              , tAggr
              , tCross
              , tEqJoin
              , tThetaJoin
              , tSemiJoin
              , tAntiJoin
              , tDisjUnion
              , tDifference
              ]
  where
    joinInfo          = [("a", "c", A.EqJ), ("b", "d", A.LeJ)]
    sortInfo          = [("a", A.Asc)]
    colTypes          = [("a", A.AInt), ("b", A.AStr)]
    colTypes2         = [("c", A.AInt), ("d", A.AInt)]
    mapping op        = [ (0, C.NullaryOp (A.LitTable [] colTypes))
                        , (1, C.NullaryOp (A.LitTable [] colTypes2))
                        , (2, op)
                        ]
    singletonGraph :: A.PFAlgebra -> T.PFDag
    singletonGraph op = D.mkDag ( IntMap.fromList $ mapping op
                                )
                                [2]
    -- nullary operators
    singletonN    = singletonGraph . C.NullaryOp
    tLitTable     =
        singletonN $ A.LitTable [[A.VInt 0, A.VStr "test"]] colTypes
    tEmptyTable   = singletonN $ A.LitTable [] colTypes
    tTableRef     = singletonN $ A.TableRef ("foo", colTypes, [])
    
    -- unary operators
    singletonU op = singletonGraph $ C.UnOp op 0

    tRowNum       = singletonU (A.RowNum ("c", sortInfo, Just "b"))
    tRowRank      = singletonU (A.RowRank ("c", sortInfo))
    tRank         = singletonU (A.Rank ("c", sortInfo))
    tProject      =
        singletonU
        $ A.Project [ ("x", A.ConstE $ A.VInt 0)
                    , ("y", A.ColE "a")
                    , ("z", A.UnAppE A.Not
                                     $ A.ConstE $ A.VBool False
                      )
                    ]
          -- TODO
    tSelect       =
        singletonU $ A.Select $ A.ConstE $ A.VBool True
    tDistinct     = singletonU $ A.Distinct ()
    tAggr         =
        singletonU
        $ A.Aggr ([(A.Count, "count")], [("a", A.ColE "a")])

    -- binary operators
    singletonB op = singletonGraph $ C.BinOp op 0 1

    tCross        = singletonB $ A.Cross ()
    tEqJoin       = singletonB $ A.EqJoin ("a", "c")
    tThetaJoin    = singletonB $ A.ThetaJoin joinInfo
    tSemiJoin     = singletonB $ A.SemiJoin joinInfo
    tAntiJoin     = singletonB $ A.AntiJoin joinInfo
    tDisjUnion    = singletonB $ A.DisjUnion ()
    tDifference   = singletonB $ A.Difference ()


data Options = Options
            { optDot       :: Bool
            , optRenderDot :: Bool
            , optDebug     :: Bool
            , optHelp      :: Bool
            , optMatFun    :: MatFun
            , optFast      :: Bool
            , optDebugFun  :: Maybe (T.PFDag -> MatFun -> String)
            }
defaultOptions :: Options
defaultOptions = Options False False False False CTE.materialize False Nothing

-- idea from VLToX100.hs from DSH
options :: [OptDescr (Options -> Options)]
options = [ Option
            "p"
            ["print-dot"]
            (NoArg (\opt -> opt { optDot = True }))
            "Output each read directed acyclic graph as dot file"
          , Option
            "r"
            ["render-dot"]
            (NoArg (\opt -> opt { optDot = True, optRenderDot = True }))
            "Render each read directed acyclic graph as png"
          , Option
            "d"
            ["debug"]
            (OptArg handleDebug "<flags>")
            "Show debug output, where optional arguments can be\n\
            \composed of (trigger analyze and/or explain):\n\
            \    a | e"
          , Option
            "h"
            ["help"]
            (NoArg (\opt -> opt { optHelp = True }))
            "Show help"
          , Option
            "m"
            ["mat-strategy"]
            (ReqArg (\s opt -> opt { optMatFun = parseMatFun s }) "<strategy>")
            "Specify the type of materialization (defaults to cte):\n\
            \    cte | tmp | com | coml | comh"
          , Option
            "f"
            ["fast"]
            (NoArg (\opt -> opt { optFast = True }))
            "Render a fast but ugly sql representation"
          ]
        where parseMatFun s = case s of
                  "cte"  -> CTE.materialize
                  "tmp"  -> TemporaryTable.materialize
                  "com"  -> Combined.materialize
                  "coml" ->
                      Combined.materializeByBindingStrategy Combined.Lowest
                  "comh" ->
                      Combined.materializeByBindingStrategy Combined.Highest
                  -- TODO stupid library is not able to parse sub args
                  _     -> CTE.materialize

              handleDebug optArg opts =
                  ( case optArg of
                       Nothing -> opts
                       Just os ->
                           opts { optDebugFun = Just $ parseDebugStr os }
                  )
                  { optDebug = True }

              parseDebugStr os = renderAdvancedDebugOutput ('e' `elem` os)
                                                           $ 'a' `elem` os


main :: IO ()
main = do
    args <- getArgs

    let (funs, realArgs, errs) = getOpt RequireOrder options args
        usedOptions            = foldr ($) defaultOptions funs

    case (optHelp usedOptions, not $ null errs) of
        -- not used the help option and no parse errors
        (False, False)     ->  do
            let debug    = optDebug usedOptions
                matFun   = optMatFun usedOptions
                output d = case optDebugFun usedOptions of
                    Just f ->
                        putStrLn $ f d matFun
                    Nothing ->
                        putShowSLn $ renderDebugOutput d matFun debug

            case realArgs of
                filenames@(_:_) ->
                    mapM_ process filenames
                  where process filename = do
                            mDag <- readDagFromFile filename

                            case mDag of
                                Left err  ->
                                    putStrLn err
                                Right dag ->

                                    if optDot usedOptions
                                    then do
                                        let dotPath = filename ++ ".dot"
                                            pdfPath = filename ++ ".pdf"

                                        outputDot dotPath dag
                                        
                                        when (optRenderDot usedOptions)
                                            $ renderDot dotPath pdfPath
                                    else if optFast usedOptions
                                         then putShowSLn $ renderOutput dag matFun
                                         else output dag
                []              ->
                    -- Run tests 
                    forM_ testGraphs $ \d -> output d

        -- show help when requested or wrong arguments given
        (_, hasInvalidArgs) -> do
            when hasInvalidArgs
                 $ putStrLn $ "Invalid args: \n" ++ concatMap ("    " ++) errs
            putStrLn $ usageInfo "Usage: Test [options] [files]" options

