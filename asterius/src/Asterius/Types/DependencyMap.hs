{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Asterius.Types.DependencyMap
-- Copyright   :  (c) 2018 EURL Tweag
-- License     :  All rights reserved (see LICENCE file in the distribution).
--
-- TODO: Explain.
module Asterius.Types.DependencyMap
  ( DependencyMap,
    toDependencyMap,
    (!),
  )
where

import Asterius.Types.EntitySymbol
import Asterius.Types.SymbolMap (SymbolMap)
import Asterius.Types.SymbolSet (SymbolSet)
import Binary
import Control.DeepSeq
import Data.Coerce
import Data.Data
import GHC.Stack

newtype DependencyMap = DependencyMap {fromDependencyMap :: SymbolMap SymbolSet}
  deriving newtype (Eq, Semigroup, Monoid, NFData, Binary)
  deriving stock (Data)

instance Show DependencyMap where
  showsPrec = coerce (showsPrec @(SymbolMap SymbolSet))

-- ----------------------------------------------------------------------------

{-# INLINE toDependencyMap #-}
toDependencyMap :: SymbolMap SymbolSet -> DependencyMap
toDependencyMap = coerce

-- | /O(min(n,W))/. Find the value at an 'EntitySymbol'. Calls 'error' when the
-- element can not be found.
{-# INLINE (!) #-}
(!) :: HasCallStack => DependencyMap -> EntitySymbol -> SymbolSet
(!) = coerce (!)

infixl 9 !
