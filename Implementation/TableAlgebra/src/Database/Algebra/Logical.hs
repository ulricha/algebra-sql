{-| 
This package provides a convenient interface to construct Table Algebra
plans that can be dealt with by Pathfinder
(http://www-db.informatik.uni-tuebingen.de/research/pathfinder). 
A describtion of the algebra can be found at: 
http://dbworld.informatik.uni-tuebingen.de/projects/pathfinder/wiki/Logical_Algebra
This module only provides a subset of the complete algebra.
-}

module Database.Algebra.Logical (
    AlgPlan,
    union, attach, proj, getLoop, rownum, rownum', eqJoin, rank, eqTJoin, distinct, rowrank, cast, difference, aggr,
    select, posSelect, dbTable, notC, cross, oper, emptyTable, tag, litTable,
    withBinding, withContext, getGamma, fromGam, 
    nat, int, bool, double, string,
    natT, intT, surT, boolT, doubleT, stringT,
    SortDir(..), AggrType(..),
    Column(..), Columns, 
    ATy(..),
    SchemaInfos, KeyInfos, AlgNode, GraphM, Gam,
    initLoop, runGraph, ProjPair, ProjInf,
    module Database.Algebra.Logical.Monadic.Create)where

import Database.Algebra.Logical.Data.Algebra
import Database.Algebra.Logical.Data.Create
import Database.Algebra.Graph.GraphBuilder
import Database.Algebra.Logical.Monadic.Create
