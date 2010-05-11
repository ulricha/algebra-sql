module Ferry.Compiler.Compile where
    
import Ferry.Compiler.Types
import Ferry.Front.Parser.Parser
import Ferry.Front.Render.Pretty

import System.FilePath.Posix(takeFileName)

-- | The compiler pipeline
--   Note that there should be a monadic style for handling all the steps in the pipeline
compile :: Config -> [String] -> IO ()
compile opts inp = do
                        src <- case (input opts) of
                                File -> readFile $ head inp
                                Arg  -> return $ unlines inp
                        let file = case (input opts) of
                                    File -> takeFileName $ head inp
                                    Arg  -> "StdIn"
                        let ast = parseFerry file src
                        case ast of
                            (Left e) -> putStrLn $ show e
                            (Right a) -> putStrLn $ prettyAST a
                        --let pretty = prettyAST ast 
                        --putStrLn pretty -- $ show ast
                        return ()
                        --putStrLn (show $ length tokens)