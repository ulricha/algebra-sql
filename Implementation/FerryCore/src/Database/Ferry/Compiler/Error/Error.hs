-- | Internal compiler errors
module Database.Ferry.Compiler.Error.Error where

import Control.Monad.Error
import Database.Ferry.TypedCore.Data.Type

import Text.ParserCombinators.Parsec (ParseError())

-- | The FerryError datatype represents errors that occur during compilation
data FerryError = NoSuchFile String
                | ParserError ParseError
                | UnificationError FType FType
                | UnificationRecError [(RLabel, FType)] [(RLabel, FType)]
                | ClassAlreadyDefinedError String
                | SuperClassNotDefined String [String]
                | ClassNotDefined String
                | RecordDuplicateFields (Maybe String) [(RLabel, FType)]
                | NotARecordType FType
                | RecordWithoutI FType String
                | UnificationOfRecordFieldsFailed RLabel RLabel
                | UnificationFail RLabel RLabel
                | ProcessComplete
        deriving Show
                
-- | Just to satisfy the Error monad 
instance Error FerryError where
    noMsg = error "This function should not be used Error.hs noMsg"
    strMsg = error "This function should not be used Error.hs strMsg"

-- | Print an error message    
handleError :: FerryError -> IO ()
handleError ProcessComplete = return () -- Process complete just means everything was fine but the pipeline was ordered to stop early
handleError (ParserError e) = putStrLn $ show e
handleError e               = putStrLn $ show e
     