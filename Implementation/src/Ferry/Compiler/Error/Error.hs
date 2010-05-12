module Ferry.Compiler.Error.Error where

import Control.Monad.Error

data FerryError = NoSuchFile String
                | ProcessComplete
 
instance Error FerryError where
    noMsg = error "This function should not be used Error.hs noMsg"
    strMsg = error "This function should not be used Error.hs strMsg"
    

     