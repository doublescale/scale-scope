{-# LANGUAGE TemplateHaskell #-}

module AppState where

import Control.Lens (makeLensesWith)
import Linear (V2, V3)

import Util (suffixedLRule)

data AppState renderstate = AppState
  { appWinSize :: V2 Int
  , appPaused :: Bool
  , appFrame :: Double
  , appFrameRateInterp :: Double
  , appFrameRate :: Double
  , appFrameRateFactor :: Double
  , appTimePrev :: Double
  , appViewState :: ViewState
  , appRenderState :: renderstate
  }

data ViewState = ViewState
  { viewCamAngle :: V2 Double
  , viewCamAngleVel :: V2 Double
  , viewCamDistance :: Double
  , viewCamDistanceVel :: Double
  , viewCamPos :: V3 Double
  , viewSamplePos :: V3 Double
  }

makeLensesWith suffixedLRule ''AppState

makeLensesWith suffixedLRule ''ViewState
