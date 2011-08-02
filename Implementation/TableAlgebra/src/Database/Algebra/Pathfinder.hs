{-| 
This package provides a convenient interface to construct Table Algebra
plans that can be dealt with by Pathfinder
(http://www-db.informatik.uni-tuebingen.de/research/pathfinder). 
A describtion of the algebra can be found at: 
http://dbworld.informatik.uni-tuebingen.de/projects/pathfinder/wiki/Pathfinder_Algebra
This module only provides a subset of the complete algebra.
-}

module Database.Algebra.Pathfinder (
    union, attach, proj, rownum, rownum', eqJoin, rank, eqTJoin, distinct, rowrank, cast, difference, aggr,
    select, posSelect, dbTable, notC, cross, oper, emptyTable, tag, litTable, litTableSingle,
    nat, int, bool, double, string,
    natT, intT, surT, boolT, doubleT, stringT,
    SortDir(..), AggrType(..),
    Column(..), Columns, 
    ATy(..), AVal(..),
    SchemaInfos, KeyInfos, ProjInf,
    PFAlgebra,
    initLoop,
    module Database.Algebra.Pathfinder.Monadic.Create)where

import Database.Algebra.Pathfinder.Data.Algebra
import Database.Algebra.Pathfinder.Data.Create
import Database.Algebra.Pathfinder.Monadic.Create
