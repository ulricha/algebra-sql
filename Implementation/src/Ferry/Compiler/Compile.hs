module Ferry.Compiler.Compile (compile) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Front.Parser.Parser
import Ferry.Front.Render.Pretty


import Ferry.Compiler.Stages.ReadStage
import Ferry.Compiler.Stages.ParseStage
import Ferry.Compiler.Stages.NormaliseStage
import Ferry.Compiler.Stages.ToCoreStage
import Ferry.Compiler.Stages.TypeInferStage
import Ferry.Compiler.Stages.RewriteStage
import Ferry.Compiler.Stages.BoxingStage
import Ferry.Compiler.Stages.ToAlgebraStage

import System.FilePath.Posix(takeFileName)
import System.IO

-- | The compiler pipeline
--   Note that there should be a monadic style for handling all the steps in the pipeline
compile :: Config -> [String] -> IO ()
compile opts inp = do
                        src <- case (input opts) of
                                File f -> readFile f
                                Arg  -> do
                                            src <- getContents
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
                            (Left e) -> handleError e
                            (Right ()) -> return ()

-- | Write a file, if the given filename is Nothing write to stdout.
--   In the case of the Just x write to x
outputFile :: File -> IO ()
outputFile (f, b) = 
                    do
                     (h, fm) <- case f of
                            Nothing -> return (stdout, False)
                            Just f  -> do
                                        h <- openFile f WriteMode
                                        return (h, True)
                     hPutStrLn h b
                     hFlush h
                     if fm then hClose h else return ()
                    

-- | The compiler pipeline. The given string is transformed dependent on the configuration of the Phaseresult
--   monad.
pipeline :: String -> PhaseResult ()
pipeline src = readPhase src >>=
                 parsePhase >>=
                 normalisePhase >>=
                 toCorePhase >>=
                 typeInferPhase >>=
                 rewritePhase >>=
                 boxingPhase >>=
                 algebraPhase >>
                 return () 