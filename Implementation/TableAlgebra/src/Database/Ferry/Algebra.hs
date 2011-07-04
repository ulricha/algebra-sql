{-| 
This package provides a convenient interface to construct Table Algebra
plans that can be dealt with by Pathfinder
(http://www-db.informatik.uni-tuebingen.de/research/pathfinder). 
A describtion of the algebra can be found at: 
http://dbworld.informatik.uni-tuebingen.de/projects/pathfinder/wiki/Logical_Algebra
This module only provides a subset of the complete algebra.
-}

module Database.Ferry.Algebra (
    AlgPlan,
    union, attach, proj, getLoop, rownum, rownum', eqJoin, rank, eqTJoin, distinct, rowrank, cast, difference, aggr,
    select, posSelect, dbTable, notC, cross, oper, emptyTable, tag, litTable, litTable',
    withBinding, withContext, getGamma, fromGam, 
    nat, int, bool, double, string,
    natT, intT, surT, boolT, doubleT, stringT,
    SortDir(..), AggrType(..),
    Column(..), Columns, 
    ATy(..),
    SchemaInfos, KeyInfos, AlgNode, GraphM, Gam,
    initLoop, runGraph, ProjPair, ProjInf,
    module Database.Ferry.Algebra.Monadic.Create)where

import Database.Ferry.Algebra.Data.Algebra
import Database.Ferry.Algebra.Data.Create
import Database.Ferry.Algebra.Data.GraphBuilder
import Database.Ferry.Algebra.Monadic.Create
