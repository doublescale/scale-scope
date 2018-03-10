{-# OPTIONS_GHC -fno-warn-orphans #-}

module SerializeHalf () where

import Data.Serialize (Serialize, get, put)
import Foreign.C.Types (CUShort(CUShort))
import Numeric.Half (Half(Half))

instance Serialize Half where
  get = Half . CUShort <$> get
  put (Half (CUShort w16)) = put w16
