-- | This module wraps the parsing stage, transforming a string into a ferry front AST
module Ferry.Compiler.Stages.ParseStage (parsePhase) where
    
import Ferry.Compiler.Types
import Ferry.Compiler.Error.Error
import Ferry.Compiler.ExecuteStep

import Ferry.Front.Parser.Parser
import Ferry.Front.Data.Language
import Ferry.Front.Render.Pretty()

import Ferry.Common.Render.Pretty


parsePhase :: String -> PhaseResult Expr
parsePhase s = executeStep parseStage s

parseStage :: CompilationStep String Expr
parseStage = CompilationStep "Parse"  Parse step artefacts
    where
        step :: String -> PhaseResult Expr
        step args = do
                        opts <- getConfig
                        let fileName = case input opts of
                                 Arg -> "StdIn"
                                 File f -> f
                            pr = parseFerry fileName args
                         in case pr of
                             Left err -> newError $ ParserError err
                             Right ex -> return ex
        artefacts = [(PrettyAST, "ferry", \s -> return $ prettyPrint s)]