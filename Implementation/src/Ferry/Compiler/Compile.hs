module Ferry.Compiler.Compile where
    
import Ferry.Compiler.Types
import Ferry.Front.Parser.Parser
import Ferry.Front.Render.Pretty

import Ferry.Compiler.Stages.ReadStage
import Ferry.Compiler.Stages.ParseStage
import Ferry.Compiler.Stages.NormaliseStage
import Ferry.Compiler.Stages.ToCoreStage

import System.FilePath.Posix(takeFileName)
import System.IO

-- | The compiler pipeline
--   Note that there should be a monadic style for handling all the steps in the pipeline
compile :: Config -> [String] -> IO ()
compile opts inp = do
                        src <- case (input opts) of
                                File f -> readFile f
                                Arg  -> do
                                            src <- getLine
                                            return src
                        let file = case (input opts) of
                                    File f -> takeFileName f
                                    Arg  -> "StdIn"
                        let (r, l, f) = runPhase opts $ pipeline src   
                        sequence $ map outputFile f
                        if (debug opts)
                            then putStrLn $ unlines l
                            else return ()
                        case r of
                            (Left e) -> putStrLn $ show e
                            (Right ()) -> return ()
                        return ()

outputFile :: File -> IO ()
outputFile (f, b) = 
                    do
                     h <- case f of
                            Nothing -> return stdout
                            Just f  -> openFile f WriteMode
                     hPutStrLn h b
                     hFlush h
                     hClose h
                    
                        
pipeline :: String -> PhaseResult ()
pipeline src = readPhase src >>=
                 parsePhase >>=
                 normalisePhase >>=
                 toCorePhase >>=
                 \_ -> return () 