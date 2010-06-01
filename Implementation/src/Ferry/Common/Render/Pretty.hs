module Ferry.Common.Render.Pretty where
    
import qualified Data.List as L
class Pretty a where
    pretty :: a -> Int -> String

prettyPrint :: Pretty a => a -> String
prettyPrint e = pretty e 0

newLine :: Int -> String
newLine n = "\n" ++ (take n $ repeat ' ')

mapIntersperseConcat :: (a -> [b]) -> [b] -> [a] -> [b]
mapIntersperseConcat f e l = concat $ L.intersperse e $ map f l

intersperseComma :: Pretty a => [a] -> Int -> String
intersperseComma xs i = concat $ L.intersperse ", " $ map (flip pretty i) xs