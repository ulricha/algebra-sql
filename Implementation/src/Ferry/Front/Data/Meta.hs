module Ferry.Front.Data.Meta where
	
import Text.ParserCombinators.Parsec.Pos

data Meta = Meta {startPos::SourcePos}

instance Show Meta where
    show _ = ""

class HasMeta a where
    getMeta :: a -> Meta
    getPos  :: a -> SourcePos
    getPos a = startPos $ getMeta a 
    