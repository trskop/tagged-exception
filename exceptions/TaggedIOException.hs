{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:       $HEADER$
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Meaning of 'IOException' depends on a context. In a lot of situations one
-- needs to create newtype wrapper for it just to add that semantical layer on
-- top of it. Phantom types are a way how to get around the necessity of
-- creating wrapper. Instead one creates phantom type, which may be empty data
-- declaration, and makes it instance of 'IOExceptionTag'. Optionally providing
-- specific conversion to error message.
module TaggedIOException
  where

import Control.Exception (Exception, IOException)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(showsPrec), ShowS)


newtype TaggedIOException t = TaggedIOException IOException
  deriving (Eq, Generic, Generic1, Typeable)

class IOExceptionTag t where
    -- | Show 'TaggedIOException' depending on the tag type. Useful in cases
    -- when one needs to modify error message to better convey semantics.
    --
    -- Default implemetation is in terms of 'showsPrec' as it is defined for
    -- 'IOException':
    --
    -- @
    -- 'showsPrecIOException' n (TaggedIOException e) = showsPrec n e
    -- @
    showsPrecIOException :: Int -> TaggedIOException t -> ShowS
    showsPrecIOException n (TaggedIOException e) = showsPrec n e

-- | @'showsPrec' = 'showsPrecIOException'@
instance IOExceptionTag t => Show (TaggedIOException t) where
    showsPrec = showsPrecIOException

instance
    ( IOExceptionTag t
    , Typeable (TaggedIOException t)
    )
    => Exception (TaggedIOException t)
