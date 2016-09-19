-- This module defines different SQL dialects.
module Database.Algebra.SQL.Dialect
    ( Dialect(..)
    , forMonetDB
    , forPostgreSQL
    ) where

-- TODO Provide feature specific records, in case this file gets bigger.

-- | Defines the possible SQL dialects used for certain tasks like rendering and
-- materialization.
data Dialect = SQL99
             | PostgreSQL
             | MonetDB

forMonetDB :: Dialect -> Bool
forMonetDB MonetDB = True
forMonetDB _       = False

forPostgreSQL :: Dialect -> Bool
forPostgreSQL PostgreSQL = True
forPostgreSQL _          = False
