{-| 
This package provides a convenient interface to construct Table Algebra
plans that can be dealt with by Pathfinder
(http://www-db.informatik.uni-tuebingen.de/research/pathfinder). 
A describtion of the algebra can be found at: 
http://dbworld.informatik.uni-tuebingen.de/projects/pathfinder/wiki/Logical_Algebra
This module only provides a subset of the complete algebra.
-}

module Ferry.Algebra (
    AlgPlan,
    transform,
    union, emptyPlan, attach, proj, getLoop, subPlan, rownum, rownum', eqJoin, rank, eqTJoin, distinct, rowrank, cast, difference, aggr,
    select, posSelect, dbTable, notC, cross, oper, emptyTable,
    withBinding, withContext, getGamma, getPlan, fromGam, 
    nat, int, bool, double, string,
    natT, intT, surT, boolT, doubleT, stringT,
    SortDir(..), AggrType(..),
    SubPlan(..), AlgRes,
    Column(..), Columns, 
    ATy(..),
    SchemaInfos, KeyInfos, AlgNode, GraphM, Gam,
    initLoop, runGraph)where

import Ferry.Algebra.Data.Algebra
import Ferry.Algebra.Data.Create
import Ferry.Algebra.Data.GraphBuilder
import Ferry.Algebra.Render.XML
