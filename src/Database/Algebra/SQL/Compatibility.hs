-- This module defines compatibility modes, for different SQL dialects.
module Database.Algebra.SQL.Compatibility
    ( CompatMode(..)
    ) where

-- TODO Provide feature specific records, in case this file gets bigger.

-- | Defines the possible modes used for certain tasks like rendering and
-- materialization.
data CompatMode = SQL99
                | PostgreSQL

