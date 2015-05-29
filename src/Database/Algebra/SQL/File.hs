-- | Provides file tools used for outputting and converting JSON plan
-- files.
module Database.Algebra.SQL.File where

import           Control.Monad.Error.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8        as BL

import           System.FilePath
import           System.Process

import qualified Database.Algebra.Dag              as D
import           Database.Algebra.Table.Render.Dot

import qualified Database.Algebra.SQL.Tile         as T

readDagFromFile :: FilePath -> IO (Either String T.TADag)
readDagFromFile filename = case takeExtension filename of
    ".plan" -> do
        -- FIXME This function is a mess, does an unchecked fromJust, which
        -- easily fails.
        Just dag <- decode <$> BL.readFile filename

        return $ return dag

    format ->
        return $ throwError $ "unkown file format '" ++ format ++ "'"

outputDot :: FilePath -> T.TADag -> IO ()
outputDot filename dag = do
    writeFile filename result
    putStrLn $ "Writing dot file to '" ++ filename ++ "'"
  where result = renderTADot (D.rootNodes dag) (D.nodeMap dag)

renderDot :: FilePath -> FilePath -> IO ()
renderDot dotPath pdfPath = do
    putStrLn $ "Rendering dot file to '" ++ pdfPath ++ "'"
    _ <- rawSystem "dot" ["-Tpdf", "-o", pdfPath, dotPath]
    return ()
