{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-| Everything related to untyped core -}
module Ferry.Syntax {-# DEPRECATED "Use Ferry.SyntaxTyped instead of this module. This module will not be available in future FerryCore releases." #-}
(
Ident,
Identifier, Const (..), VarContainer(..),
Op (..), CoreExpr (..), RecElem (..), Param (..), Column (..), Key (..), Type(..),
Dotify(..),
module Ferry.Common.Render.Pretty
)
where
import Ferry.Common.Data.Base    
import Ferry.Core.Data.Core (Op (..), CoreExpr (..), RecElem (..), Param (..), Column (..), Key (..), Ident, Type(..))
import Ferry.Core.Render.Dot()
import Ferry.Common.Render.Dot(Dotify(..))
import Ferry.Common.Render.Pretty