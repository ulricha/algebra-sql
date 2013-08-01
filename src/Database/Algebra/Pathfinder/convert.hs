import System.Environment(getArgs)
import System.Cmd(rawSystem)

import Database.Algebra.Pathfinder.JSON
import Database.Algebra.Pathfinder.Dot



-- |Gets command line argument (a plan's file name) and 
-- |converts it to a dot-able file named "dot"
-- |then invokes dot to create a pdf ("dot.pdf") with the graph 
-- |and last opens this file
main = do 
	args <- getArgs
	(tags, algnode, algebra) <- planFromFile $ head args
	let str = renderPFDot tags algnode algebra
	writeFile "dot" str
	subProgram

-- |Writes the dottext into a pdf and opens it afterwards
subProgram = do
	rawSystem "dot" ["dot", "-Tpdf", "-o", "dot.pdf"]
	rawSystem "gnome-open" ["dot.pdf"]
