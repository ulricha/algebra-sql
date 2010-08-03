{- | This module describes the meta information that is added to each node in the ferry front AST
-}
module Ferry.Front.Data.Meta where
	
import Text.ParserCombinators.Parsec.Pos

-- | Meta information is a record (so that its easily extensible)
-- | Contains information on the start position of the element
data Meta = Meta {startPos::SourcePos}
 deriving (Show, Eq)

-- | Create an empty position element, for generated AST nodes
emptyPos :: SourcePos
emptyPos = initialPos ""

-- | Meta information containing empty position
emptyMeta :: Meta
emptyMeta = Meta emptyPos

-- | Type class for retrieving meta information
-- | Minimal complete instance provides getMeta
-- | getPos returns position element from meta information
class HasMeta a where
    getMeta :: a -> Meta
    getPos  :: a -> SourcePos
    getPos a = startPos $ getMeta a 
    