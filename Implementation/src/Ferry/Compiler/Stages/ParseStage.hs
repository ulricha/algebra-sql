module Ferry.Compiler.Stages.ParseStage (parsePhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Front.Parser.Parser
import Ferry.Front.Data.Language
import Ferry.Front.Render.Pretty

import Ferry.Common.Render.Pretty

import Text.ParserCombinators.Parsec (ParseError(..))

parsePhase :: String -> PhaseResult Expr
parsePhase s = executeStep parseStage s

parseStage :: CompilationStep String Expr
parseStage = CompilationStep "Parse"  Parse step artefacts
    where
        step :: String -> PhaseResult Expr
        step arg = do
                        opts <- getConfig
                        let fileName = case input opts of
                                 Arg -> "StdIn"
                                 File f -> f
                            pr = parseFerry fileName arg
                         in case pr of
                             Left err -> newError $ ParserError err
                             Right expr -> return expr
        artefacts = [(PrettyAST, "ferry", \s -> return $ prettyPrint s)]