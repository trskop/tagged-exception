{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       $HEADER$
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
module ExceptionSet
  where

import Control.Exception (Exception)
import Data.Bool (Bool(False, True))
import Data.Function ((.))
import Data.Coerce (coerce)

import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch as Exception
    ( MonadCatch(catch)
    , MonadThrow(throwM)
    )
import Control.Monad.TaggedException.Internal.Throws (Throws(Throws))


type family Insert e set where
    Insert e '[]      = e ': '[]
    Insert e (e ': s) = Insert e s      -- Removing any possible duplicities.
    Insert e (a ': s) = a ': Insert e s

type family Elem e set where
    Elem e '[]      = 'False
    Elem e (e ': s) = 'True
    Elem e (a ': s) = Elem e s

type family Empty eSet where
    Empty '[]      = 'True
    Empty (a ': s) = 'False

type family Remove e set where
    Remove e '[]      = '[]
    Remove e (e ': s) = Remove e s      -- Removing any possible duplicities.
    Remove e (a ': s) = a ': Remove e s

elemT :: (Elem e eSet ~ 'True) => Throws e m a -> Throws eSet m a
elemT (Throws x) = Throws x

insertT :: proxy e -> Throws eSet m a -> Throws (Insert e eSet) m a
insertT _proxy (Throws x) = Throws x

extractT :: Throws '[] m a -> m a
extractT (Throws x) = x

-- | Following type checks:
--
-- @
-- catch (throw (undefined :: IOException) :: Throws '[IOException] IO ())
--      (undefined :: IOException -> IO ())
--      :: Throws '[] IO ()
-- @
catch
    ::  ( Exception e
        , MonadCatch m
        , Elem e eSet ~ 'True
        , Elem e (Remove e eSet) ~ 'False
        )
    => Throws eSet m a
    -> (e -> m a)
    -> Throws (Remove e eSet) m a
catch (Throws x) = Throws . Exception.catch x

-- | Following type checks:
--
-- @
-- throw (undefined :: IOException) :: Throws '[IOException] IO ()
-- @
--
-- @
-- throw (undefined :: IOException)
--     :: Throws '[AssertionFailed, IOException] IO ()
-- @
--
-- This doesn't type check:
--
-- @
-- throw (undefined :: IOException) :: Throws '[] IO ()
-- @
--
-- @
-- throw (undefined :: IOException) :: Throws '[AssertionFailed] IO ()
-- @
throw
    ::  ( Exception e
        , MonadThrow m
        , Elem e eSet ~ 'True
        )
    => e -> Throws eSet m a
throw = Throws . Exception.throwM

-- {{{ Catching ---------------------------------------------------------------

type family Catching e t where
    Catching e (Throws '[e] m) = m
    Catching e (Throws eSet m) = Throws (Remove e eSet) m

class EvalThrows t1 t2 where
    evalThrows :: t1 a -> t2 a

instance EvalThrows (Throws '[] m) m where
    evalThrows = coerce

instance EvalThrows (Throws eSet m) (Throws eSet m) where
    evalThrows = coerce

catching
    ::  ( Exception e
        , MonadCatch m
        , Elem e eSet ~ 'True
        , Elem e (Remove e eSet) ~ 'False
        , EvalThrows (Throws (Remove e eSet) m) (Catching e (Throws eSet m))
        )
    => Throws eSet m a
    -> (e -> m a)
    -> Catching e (Throws eSet m) a
catching = (evalThrows .) . catch

-- }}} Catching ---------------------------------------------------------------
