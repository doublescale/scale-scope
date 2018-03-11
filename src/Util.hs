module Util where

import Control.Lens
       (LensRules, (&), (.~), lensField, lensRules, mappingNamer)
import Foreign.C (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

suffixedLRule :: LensRules
suffixedLRule = lensRules & lensField .~ mappingNamer (\x -> [x ++ "L"])

fromCString :: CString -> String
fromCString = unsafePerformIO . peekCString
