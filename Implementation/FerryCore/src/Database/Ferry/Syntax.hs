{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-| Everything related to untyped core -}
module Database.Ferry.Syntax {-# DEPRECATED "Use Database.Ferry.SyntaxTyped instead of this module. This module will not be available in future FerryCore releases." #-}
(
Ident,
Identifier, Const (..), VarContainer(..),
Op (..), CoreExpr (..), RecElem (..), Param (..), Column (..), Key (..), Type(..),
Dotify(..),
module Database.Ferry.Common.Render.Pretty
)
where
import Database.Ferry.Common.Data.Base    
import Database.Ferry.Core.Data.Core (Op (..), CoreExpr (..), RecElem (..), Param (..), Column (..), Key (..), Ident, Type(..))
import Database.Ferry.Core.Render.Dot()
import Database.Ferry.Common.Render.Dot(Dotify(..))
import Database.Ferry.Common.Render.Pretty