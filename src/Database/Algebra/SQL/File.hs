-- | Provides file tools used for outputting and converting xml and
-- json plan files.
module Database.Algebra.SQL.File where

import           Control.Monad.Error.Class
import qualified Data.IntMap                        as IntMap
import           System.FilePath
import           System.IO                          
import           System.Process                     

import qualified Database.Algebra.Dag               as D
import           Database.Algebra.Table.Render.Dot  (renderTADot)
import qualified Database.Algebra.Table.Render.JSON as JSON

import qualified Database.Algebra.SQL.Tile          as T

readDagFromFile :: FilePath -> IO (Either String T.TADag)
readDagFromFile filename = case takeExtension filename of
    -- ".xml"  -> do
    --     s <- readFile filename
    --
    --     case XML.deserializeQueryPlan filename s of
    --
    --         Left es   ->
    --             return $ throwError $ "could not deserialize "
    --                                   ++ filename
    --                                   ++ ":\n"
    --                                   ++ errorMessages
    --               where errorMessages  = concatMap errorLineFun es
    --                     errorLineFun e = "    " ++ e ++ "\n"
    --         Right dag ->
    --             return $ return dag
    --
    ".plan" -> do
        -- FIXME This function is a mess, does an unchecked fromJust, which
        -- easily fails.
        (_, rootNodes, nodeMap) <- JSON.planFromFile filename

        return $ return $ D.mkDag nodeMap rootNodes

    format ->
        return $ throwError $ "unkown file format '" ++ format ++ "'"

outputDot :: FilePath -> T.TADag -> IO ()
outputDot filename dag = do
    writeFile filename result
    putStrLn $ "Writing dot file to '" ++ filename ++ "'"
  where result = renderTADot IntMap.empty (D.rootNodes dag) (D.nodeMap dag)

renderDot :: FilePath -> FilePath -> IO ()
renderDot dotPath pdfPath = do
    putStrLn $ "Rendering dot file to '" ++ pdfPath ++ "'"
    _ <- rawSystem "dot" ["-Tpdf", "-o", pdfPath, dotPath]
    return ()

-- Runs the Pathfinder optimizer on a XML file.
optimizeXML :: FilePath -> FilePath -> FilePath -> IO ()
optimizeXML inputFile tmpFile outputFile = do

    -- Can't rely on pfopt reading from pipe.
    hFormatted <- openFile tmpFile WriteMode
    hOutput <- openFile outputFile WriteMode
    -- pfopt only accepts formatted XML.
    (_, _, _, phXmllint) <-
        createProcess (proc "xmllint" ["--format", inputFile])
                      { std_out = UseHandle hFormatted }
    waitForProcess phXmllint
    hClose hFormatted
    (_, _, _, phOpt) <-
        createProcess (proc "pfopt" [tmpFile])
                      { std_out = UseHandle hOutput }
    waitForProcess phOpt
    hClose hOutput

