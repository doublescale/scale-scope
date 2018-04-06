{-# LANGUAGE OverloadedStrings #-}

module ParseModState
  ( parseModifiers
  ) where

import qualified Data.Attoparsec.Text as P
import Data.Functor

import Event.ModState (ModState(..), emptyModState, orModState)

parseModifiers :: P.Parser ModState
parseModifiers = foldr orModState emptyModState <$> P.many' pAnyMod

pAnyMod :: P.Parser ModState
pAnyMod = P.choice [pShift, pCtrl, pAlt]

pShift :: P.Parser ModState
pShift = "S-" $> emptyModState {modShift = True}

pCtrl :: P.Parser ModState
pCtrl = "C-" $> emptyModState {modCtrl = True}

pAlt :: P.Parser ModState
pAlt = "A-" $> emptyModState {modAlt = True}
