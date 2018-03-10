module Util where

import Control.Lens (LensRules, (&), (.~), lensField, lensRules, mappingNamer)

suffixedLRule :: LensRules
suffixedLRule = lensRules & lensField .~ mappingNamer (\x -> [x ++ "L"])
