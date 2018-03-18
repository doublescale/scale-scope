{-# LANGUAGE RankNTypes #-}

module Util where

import Control.Lens
  ( Lens'
  , LensRules
  , (&)
  , (.~)
  , assign
  , lensField
  , lensRules
  , mappingNamer
  , use
  )
import Control.Monad.State (MonadState)
import Foreign.C (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

suffixedLRule :: LensRules
suffixedLRule = lensRules & lensField .~ mappingNamer (\x -> [x ++ "L"])

fromCString :: CString -> String
fromCString = unsafePerformIO . peekCString

modifyingM :: MonadState a m => Lens' a b -> (b -> m b) -> m ()
modifyingM lns a = assign lns =<< a =<< use lns
