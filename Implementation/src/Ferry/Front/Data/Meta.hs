module Ferry.Front.Data.Meta where
	
import Text.ParserCombinators.Parsec.Pos

data Meta = Meta {startPos::SourcePos}
--   deriving Eq
 deriving (Show, Eq)

emptyPos :: SourcePos
emptyPos = initialPos ""

emptyMeta :: Meta
emptyMeta = Meta emptyPos

class HasMeta a where
    getMeta :: a -> Meta
    getPos  :: a -> SourcePos
    getPos a = startPos $ getMeta a 
    