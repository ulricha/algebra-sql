-- Provides file tools used for outputting and converting xml and json plan
-- files.
module Database.Algebra.SQL.File where

import Control.Monad.Error.Class (throwError)
import System.FilePath (takeExtension)
import System.Process
    ( rawSystem
    , createProcess
    , CreateProcess
    , std_out
    , std_in
    , StdStream(UseHandle, CreatePipe)
    , proc
    , waitForProcess
    )
import qualified Data.IntMap as IntMap
    ( empty
    )
import System.IO
    ( openFile
    , IOMode(WriteMode)
    , hClose
    )

import qualified Database.Algebra.Dag as D
--import qualified Database.Algebra.Pathfinder.Parse.XML as XML
import qualified Database.Algebra.Pathfinder.Render.JSON as JSON
import Database.Algebra.Pathfinder.Render.Dot (renderPFDot)

import qualified Database.Algebra.SQL.Tile as T

readDagFromFile :: FilePath -> IO (Either String T.PFDag)
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

outputDot :: FilePath -> T.PFDag -> IO ()
outputDot filename dag = do
    writeFile filename result
    putStrLn $ "Writing dot file to '" ++ filename ++ "'"
  where result = renderPFDot IntMap.empty (D.rootNodes dag) (D.nodeMap dag)

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

