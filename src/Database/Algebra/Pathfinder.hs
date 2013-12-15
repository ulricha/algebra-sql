{-| 
This package provides a convenient interface to construct Table Algebra
plans that can be dealt with by Pathfinder
(http://www-db.informatik.uni-tuebingen.de/research/pathfinder). 
A describtion of the algebra can be found at: 
http://dbworld.informatik.uni-tuebingen.de/projects/pathfinder/wiki/Pathfinder_Algebra
This module only provides a subset of the complete algebra.
-}

module Database.Algebra.Pathfinder (
    union, proj, rownum, rownum', eqJoin, rank, distinct, rowrank, difference, aggr,
    select, posSelect, dbTable, cross, emptyTable, litTable, litTable', thetaJoin,
    nat, int, bool, double, string,
    natT, intT, boolT, doubleT, stringT,
    SortDir(..), AggrType(..),
    Column(..), Columns, 
    ATy(..), AVal(..),
    SchemaInfos, Key, AttrName,
    PFAlgebra,
    initLoop,
    module Database.Algebra.Pathfinder.Monadic.Create)where

import Database.Algebra.Pathfinder.Data.Algebra
import Database.Algebra.Pathfinder.Data.Create
import Database.Algebra.Pathfinder.Monadic.Create
