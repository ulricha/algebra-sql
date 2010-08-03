-- | The missing instance for applicative for Parsec
module Ferry.Front.Parser.Applicative(
    module Control.Applicative
  , module Text.ParserCombinators.Parsec
  , pMeta) where
    
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import Ferry.Front.Data.Meta
import Ferry.Front.Parser.Scanner
                                      
instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap

instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus

-- | Get meta information  
pMeta :: Parser Meta
pMeta = do
            pos <- getPosition
            return (Meta pos)