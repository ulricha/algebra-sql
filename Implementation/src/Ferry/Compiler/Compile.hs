module Ferry.Compiler.Compile where
    
import Ferry.Compiler.Types
import Ferry.Front.Parser.Parser
import Ferry.Front.Render.Pretty

import Ferry.Compiler.Stages.ReadStage
import Ferry.Compiler.Stages.ParseStage
import Ferry.Compiler.Stages.NormaliseStage
import Ferry.Compiler.Stages.ToCoreStage

import System.FilePath.Posix(takeFileName)

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
                        (r, l) <- runPhase $ pipeline opts src            
                        if (debug opts)
                            then putStrLn $ unlines l
                            else return ()
                        case r of
                            (Left e) -> putStrLn $ show e
                            (Right ()) -> return ()
                        return ()

                        
pipeline :: Config -> String -> PhaseResult ()
pipeline c src = readPhase c src >>=
                 parsePhase c >>=
                 normalisePhase c >>=
                 toCorePhase c >>=
                 \_ -> return () 