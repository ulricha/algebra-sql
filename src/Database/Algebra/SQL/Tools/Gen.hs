{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                                       (forM_,
                                                                      when)
import qualified Data.IntMap                                         as IntMap (fromList)
import           System.Console.GetOpt                               (ArgDescr (NoArg, ReqArg, OptArg), ArgOrder (RequireOrder), OptDescr (Option),
                                                                      getOpt,
                                                                      usageInfo)
import           System.Environment                                  (getArgs)

import qualified Database.Algebra.Dag                                as D
import qualified Database.Algebra.Dag.Common                         as C
import qualified Database.Algebra.Table.Lang                         as A

import           Database.Algebra.SQL.Dialect
import           Database.Algebra.SQL.File
import           Database.Algebra.SQL.Materialization
import qualified Database.Algebra.SQL.Materialization.Combined       as Combined
import           Database.Algebra.SQL.Materialization.CTE            as CTE
import           Database.Algebra.SQL.Materialization.TemporaryTable as TemporaryTable
import qualified Database.Algebra.SQL.Tile                           as T
import           Database.Algebra.SQL.Util


-- test :: T.TADag
-- test = D.mkDag ( IntMap.fromList [ (0, C.BinOp (A.Cross ()) 1 2)
--                                  , (1, C.UnOp (A.Project [("x", A.ColE "y")]) 4)
--                                  , (2, C.BinOp (A.Cross ()) 1 3)
--                                  , (4, C.NullaryOp $ A.LitTable
--                                                      ( [[A.VStr "foo"]]
--                                                      , [("y", A.AStr)]
--                                                      )
--                                    )
--                                  , (3, C.UnOp (A.Project [("z", A.ColE "y")]) 4)
--                                  , (5, C.UnOp (A.Project [("x", A.ColE "y")]) 4)
--                                  ]
--                )
--                [0, 5]

-- -- TODO when using mutiple root nodes the result gets computed multiple times,
-- -- maybe save every computed tile in the state part of the transform monad for a node.
-- g1 :: T.TADag
-- g1 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Project [("result", A.ColE "str")]) 1)
--                                , (1, C.UnOp (A.Select eq) 2)
--                                , (2, C.NullaryOp $ A.LitTable
--                                                    ( [ [A.VStr "0"]
--                                                      , [A.VStr "1"]
--                                                      , [A.VStr "2"]
--                                                      ]
--                                                    , [("str", A.AStr)]
--                                                    )
--                                  )
--                                ]

--              )
--              [0]
--    where eq   = A.BinAppE A.Eq (A.BinAppE A.Plus cast $ A.ConstE $ A.VInt 41)
--                                $ A.ConstE $ A.VInt 42
--          cast = A.UnAppE (A.Cast A.AInt) $ A.ColE "str"

-- g2 :: T.TADag
-- g2 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Project [ ( "result"
--                                                          , A.BinAppE A.Plus
--                                                                      (A.ColE "a")
--                                                                      $ A.ColE "b"
--                                                          )
--                                                        ]) 2)
--                                , (2, C.BinOp (A.EqJoin ("u", "v")) 3 4)
--                                , (3, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 0, A.VInt 50]
--                                                      , [A.VInt 1, A.VInt 60]
--                                                      ]
--                                                    , [("u", A.AInt), ("a", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                , (4, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 0, A.VInt 4]
--                                                      , [A.VInt 1, A.VInt 23]
--                                                      ]
--                                                    , [("v", A.AInt), ("b", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                ]
--              )
--              [0]

-- --g3 :: T.TADag
-- --g3 = D.mkDag ( IntMap.fromList [ (0, C.BinOp (A.EqJoin ("a", "b")) 1 2)
-- --                               , (1, C.UnOp (A.Proj [("a", "u")]) 3)
-- --                               , (2, C.UnOp (A.Proj [("b", "u")]) 3)
-- --                               , (3, C.NullaryOp ( A.LitTable
-- --                                                   [ [A.VInt 0]
-- --                                                   ]
-- --                                                   [("u", A.AInt)]
-- --                                                 )
-- --                                 )
-- --                               ]
-- --             )
-- --             [0, 1, 2]
-- --
-- --g4 :: T.TADag
-- --g4 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Proj [("qsum", "qsum"), ("id", "id")]) 1)
-- --                               , (1, C.UnOp (A.Aggr ([(A.Sum "quantity", "qsum")], Just "id")) 2)
-- --                               , (2, C.NullaryOp ( A.LitTable
-- --                                                   [ [A.VInt 0, A.VInt 2]
-- --                                                   , [A.VInt 0, A.VInt 10]
-- --                                                   , [A.VInt 1, A.VInt 100]
-- --                                                   , [A.VInt 1, A.VInt 20]
-- --                                                   ]
-- --                                                   [("id", A.AInt), ("quantity", A.AInt)]
-- --                                                 )
-- --                                 )
-- --                               ]
-- --             )
-- --             [0]
-- --
-- --g5 :: T.TADag
-- --g5 = D.mkDag ( IntMap.fromList [ (0, C.UnOp (A.Proj [("y", "id")]) 1)
-- --                               , (1, C.UnOp (A.Proj [("y", "id")]) 3)
-- --                               , (2, C.UnOp (A.Proj [("y", "id")]) 3)
-- --                               , (3, C.NullaryOp ( A.TableRef
-- --                                                   ( "foo"
-- --                                                   , [ ("id", "id",  A.AInt)
-- --                                                     , ("bla", "bla",  A.AStr)
-- --                                                     ]
-- --                                                   , []
-- --                                                   )
-- --                                                 )
-- --                                 )
-- --                               ]
-- --             )
-- --             [0, 1]
-- --

-- g6 :: T.TADag
-- g6 = D.mkDag ( IntMap.fromList [ (0, C.BinOp ( A.AntiJoin [ (A.ColE "a", A.ColE "c", A.EqJ)
--                                                           , (A.ColE "b", A.ColE "d", A.LtJ)
--                                                           ]
--                                              )
--                                              1
--                                              2
--                                  )
--                                , (1, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 1, A.VInt 2]
--                                                      , [A.VInt 1, A.VInt 1]
--                                                      ]
--                                                    , [("a", A.AInt), ("b", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                , (2, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 1, A.VInt 2]
--                                                      , [A.VInt 1, A.VInt 1]
--                                                      ]
--                                                    , [("c", A.AInt), ("d", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                ]
--              )
--              [0]

-- g7 :: T.TADag
-- g7 = D.mkDag ( IntMap.fromList [ (0, C.BinOp ( A.SemiJoin [ (A.ColE "a", A.ColE "c", A.EqJ)
--                                                           , (A.ColE "b", A.ColE "d", A.LtJ)
--                                                           ]
--                                              )
--                                              1
--                                              2
--                                  )
--                                , (1, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 1, A.VInt 2]
--                                                      , [A.VInt 1, A.VInt 1]
--                                                      ]
--                                                    , [("a", A.AInt), ("b", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                , (2, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 1, A.VInt 2]
--                                                      , [A.VInt 1, A.VInt 1]
--                                                      ]
--                                                    , [("c", A.AInt), ("d", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                ]
--              )
--              [0]

-- -- Should use EXISTS due to the lack of the equal join condition.
-- g8 :: T.TADag
-- g8 = D.mkDag ( IntMap.fromList [ (0, C.BinOp ( A.SemiJoin [ (A.ColE "b", A.ColE "d", A.LtJ)
--                                                           ]
--                                              )
--                                              1
--                                              2
--                                  )
--                                , (1, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 1, A.VInt 2]
--                                                      , [A.VInt 1, A.VInt 1]
--                                                      ]
--                                                    , [("a", A.AInt), ("b", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                , (2, C.NullaryOp ( A.LitTable
--                                                    ( [ [A.VInt 1, A.VInt 2]
--                                                      , [A.VInt 1, A.VInt 1]
--                                                      ]
--                                                    , [("c", A.AInt), ("d", A.AInt)]
--                                                    )
--                                                  )
--                                  )
--                                ]
--              )
--              [0]

-- g9 :: T.TADag
-- g9 = D.mkDag ( IntMap.fromList [ (0, C.UnOp p 2)
--                                , (1, C.UnOp p 2)
--                                , (2, C.UnOp p 4)
--                                , (3, C.UnOp p 4)
--                                , (4, C.UnOp p 5)
--                                , (5, C.BinOp eq 6 7)
--                                , (6, C.UnOp (pc "a") 8)
--                                , (7, C.UnOp p 8)
--                                , (8, C.UnOp p 9)
--                                , (9, C.NullaryOp ( A.LitTable ([], [("c", A.AInt), ("d", A.AInt)] ))
--                                  )
--                                ]
--              )
--              [0, 1, 3]
--   where p    = pc "c"
--         pc c = A.Project [(c, A.ColE "c")]
--         eq   = A.EqJoin ("a", "c")

-- -- | Tests whether different binding strategies for Materialization.Combined
-- -- work.
-- g10 :: T.TADag
-- g10 = D.mkDag ( IntMap.fromList [ (0, C.BinOp eq 1 1)
--                                 , (1, C.BinOp eq 2 2)
--                                 , (2, C.UnOp p 3)
--                                 , (3, C.NullaryOp ( A.LitTable ([], [("c", A.AInt), ("d", A.AInt)] )))
--                                 ]
--               )
--               [0]
--   where p    = pc "c"
--         pc c = A.Project [(c, A.ColE "c")]
--         eq   = A.EqJoin ("c", "d")


-- testGraphs :: [T.TADag]
-- testGraphs = singleTests ++ [test, g1, g2, g6, g7, g8, g9, g10] -- g3, g4, g5]

-- -- Test for single operator translation.
-- singleTests :: [T.TADag]
-- singleTests = [ tLitTable
--               , tEmptyTable
--               , tTableRef
--               , tRowNum
--               , tRowRank
--               , tRank
--               , tProject
--               , tSelect
--               , tDistinct
--               , tAggr
--               , tCross
--               , tEqJoin
--               , tThetaJoin
--               , tSemiJoin
--               , tAntiJoin
--               , tDisjUnion
--               , tDifference
--               ]
--   where
--     joinInfo          = [(A.ColE "a", A.ColE "c", A.EqJ), (A.ColE "b", A.ColE "d", A.LeJ)]
--     sortInfo          = [(A.ColE "a", A.Asc)]
--     colTypes          = [("a", A.AInt), ("b", A.AStr)]
--     colTypes2         = [("c", A.AInt), ("d", A.AInt)]
--     mapping op        = [ (0, C.NullaryOp (A.LitTable ([], colTypes)))
--                         , (1, C.NullaryOp (A.LitTable ([], colTypes2)))
--                         , (2, op)
--                         ]
--     singletonGraph :: A.TableAlgebra -> T.TADag
--     singletonGraph op = D.mkDag ( IntMap.fromList $ mapping op
--                                 )
--                                 [2]
--     -- nullary operators
--     singletonN    = singletonGraph . C.NullaryOp
--     tLitTable     =
--         singletonN $ A.LitTable ([[A.VInt 0, A.VStr "test"]], colTypes)
--     tEmptyTable   = singletonN $ A.LitTable ([], colTypes)
--     tTableRef     = singletonN $ A.TableRef ("foo", colTypes, [])

--     -- unary operators
--     singletonU op = singletonGraph $ C.UnOp op 0

--     tRowNum       = singletonU (A.RowNum ("c", sortInfo, [A.ColE "b"]))
--     tRowRank      = singletonU (A.RowRank ("c", sortInfo))
--     tRank         = singletonU (A.Rank ("c", sortInfo))
--     tProject      =
--         singletonU
--         $ A.Project [ ("x", A.ConstE $ A.VInt 0)
--                     , ("y", A.ColE "a")
--                     , ("z", A.UnAppE A.Not
--                                      $ A.ConstE $ A.VBool False
--                       )
--                     ]
--           -- TODO
--     tSelect       =
--         singletonU $ A.Select $ A.ConstE $ A.VBool True
--     tDistinct     = singletonU $ A.Distinct ()
--     tAggr         =
--         singletonU
--         $ A.Aggr ([(A.CountStar, "count")], [("a", A.ColE "a")])

--     -- binary operators
--     singletonB op = singletonGraph $ C.BinOp op 0 1

--     tCross        = singletonB $ A.Cross ()
--     tEqJoin       = singletonB $ A.EqJoin ("a", "c")
--     tThetaJoin    = singletonB $ A.ThetaJoin joinInfo
--     tSemiJoin     = singletonB $ A.SemiJoin joinInfo
--     tAntiJoin     = singletonB $ A.AntiJoin joinInfo
--     tDisjUnion    = singletonB $ A.DisjUnion ()
--     tDifference   = singletonB $ A.Difference ()


data Options = Options
            { optDot        :: Bool
            , optRenderDot  :: Bool
            , optDebug      :: Bool
            , optHelp       :: Bool
            , optMatFun     :: MatFun
            , optFast       :: Maybe (Dialect -> T.TADag -> MatFun -> ShowS)
            , optDebugFun   :: Maybe (Dialect -> T.TADag -> MatFun -> String)
            , optDialect :: Dialect
            }
defaultOptions :: Options
defaultOptions = Options False
                         False
                         False
                         False
                         CTE.materialize
                         Nothing
                         Nothing
                         SQL99

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
            \    lcte | cte | tmp | com | coml | comh"
          , Option
            "f"
            ["fast"]
            (OptArg handleFast "<optformat>")
            "Render a fast but ugly sql representation optional with formatting:\
            \   '' | 'f'"
          , Option
            "c"
            ["compat"]
            ( ReqArg (\s opt -> opt { optDialect = parseDialect s })
                     "<mode>"
            )
            "Specify the compatibility mode (defaults to sql99):\n\
            \   sql99 | postgresql"
          ]
        where parseMatFun s = case s of
                  "lcte" -> CTE.legacyMaterialize
                  "cte"  -> CTE.materialize
                  "tmp"  -> TemporaryTable.materialize
                  "com"  -> Combined.materialize
                  "coml" ->
                      Combined.materializeByBindingStrategy Combined.Lowest
                  "comh" ->
                      Combined.materializeByBindingStrategy Combined.Highest
                  _      -> error $ "invalid materialization function '"
                                    ++ s
                                    ++ "'"

              parseDialect s = case s of
                  "sql99"      -> SQL99
                  "postgresql" -> PostgreSQL
                  _            -> error $ "invalid compatibility mode '"
                                          ++ s
                                          ++ "'"

              handleFast optArg opts =
                  opts
                  { optFast = Just $ case optArg of
                        Nothing -> renderOutputCompact
                        Just _  -> renderOutputPlain
                  }

              handleDebug optArg opts =
                  ( case optArg of
                       Nothing -> opts
                       Just os ->
                           opts { optDebugFun = Just $ parseDebugStr os }
                  )
                  { optDebug = True }

              parseDebugStr os c = renderAdvancedDebugOutput c ('e' `elem` os)
                                                               $ 'a' `elem` os


main :: IO ()
main = do
    args <- getArgs

    let (funs, realArgs, errs) = getOpt RequireOrder options args
        usedOptions            = foldr ($) defaultOptions funs

    case (optHelp usedOptions, not $ null errs) of
        -- not used the help option and no parse errors
        (False, False)     ->  do
            let debug      = optDebug usedOptions
                matFun     = optMatFun usedOptions
                compatMode = optDialect usedOptions
                output d   = case optDebugFun usedOptions of
                    Just f ->
                        putStrLn $ f compatMode d matFun
                    Nothing ->
                        putShowSLn $ renderDebugOutput compatMode d matFun debug

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
                                    else case optFast usedOptions of
                                        Just r  ->
                                            putShowSLn $ r compatMode dag matFun
                                        Nothing -> output dag
                []              -> pure ()
                    -- Run tests
                    -- forM_ testGraphs $ \d -> output d

        -- show help when requested or wrong arguments given
        (_, hasInvalidArgs) -> do
            when hasInvalidArgs
                 $ putStrLn $ "Invalid args: \n" ++ concatMap ("    " ++) errs
            putStrLn $ usageInfo "Usage: Test [options] [files]" options

