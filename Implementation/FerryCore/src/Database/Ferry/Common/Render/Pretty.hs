-- | Infrastructure for pretty printing
module Database.Ferry.Common.Render.Pretty where
    
import qualified Data.List as L

-- | Class for pretty printing a value of a.
class Pretty a where
    -- | pretty function transforms a value of a into a string with identation i.
    pretty :: a -> Int -> String

-- | Shorthand for pretty without the identation argument
prettyPrint :: Pretty a => a -> String
prettyPrint e = pretty e 0

-- | A newline followed by indenting n positions
newLine :: Int -> String
newLine n = "\n" ++ (take n $ repeat ' ')

-- | maps its first argument over the third, then intersperses
-- the result with the second argument, and finally concatenates everything.
mapIntersperseConcat :: (a -> [b]) -> [b] -> [a] -> [b]
mapIntersperseConcat f e l = concat $ L.intersperse e $ map f l

-- | Pretty print the values xs then intersperse with a comma and transform it into one string
intersperseComma :: Pretty a => [a] -> Int -> String
intersperseComma xs i = concat $ L.intersperse ", " $ map (flip pretty i) xs