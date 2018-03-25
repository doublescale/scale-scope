{-# LANGUAGE OverloadedStrings #-}

module ReadScancode
  ( readScancode
  ) where

import Data.Text (Text)
import qualified SDL

readScancode :: Text -> Maybe SDL.Scancode
readScancode x = case x of
  "0"            -> Just SDL.Scancode0
  "1"            -> Just SDL.Scancode1
  "2"            -> Just SDL.Scancode2
  "3"            -> Just SDL.Scancode3
  "4"            -> Just SDL.Scancode4
  "5"            -> Just SDL.Scancode5
  "6"            -> Just SDL.Scancode6
  "7"            -> Just SDL.Scancode7
  "8"            -> Just SDL.Scancode8
  "9"            -> Just SDL.Scancode9
  "A"            -> Just SDL.ScancodeA
  "B"            -> Just SDL.ScancodeB
  "C"            -> Just SDL.ScancodeC
  "D"            -> Just SDL.ScancodeD
  "E"            -> Just SDL.ScancodeE
  "F"            -> Just SDL.ScancodeF
  "G"            -> Just SDL.ScancodeG
  "H"            -> Just SDL.ScancodeH
  "I"            -> Just SDL.ScancodeI
  "J"            -> Just SDL.ScancodeJ
  "K"            -> Just SDL.ScancodeK
  "L"            -> Just SDL.ScancodeL
  "M"            -> Just SDL.ScancodeM
  "N"            -> Just SDL.ScancodeN
  "O"            -> Just SDL.ScancodeO
  "P"            -> Just SDL.ScancodeP
  "Q"            -> Just SDL.ScancodeQ
  "R"            -> Just SDL.ScancodeR
  "S"            -> Just SDL.ScancodeS
  "T"            -> Just SDL.ScancodeT
  "U"            -> Just SDL.ScancodeU
  "V"            -> Just SDL.ScancodeV
  "W"            -> Just SDL.ScancodeW
  "X"            -> Just SDL.ScancodeX
  "Y"            -> Just SDL.ScancodeY
  "Z"            -> Just SDL.ScancodeZ
  "Up"           -> Just SDL.ScancodeUp
  "Down"         -> Just SDL.ScancodeDown
  "Left"         -> Just SDL.ScancodeLeft
  "Right"        -> Just SDL.ScancodeRight
  "Insert"       -> Just SDL.ScancodeInsert
  "Delete"       -> Just SDL.ScancodeDelete
  "Home"         -> Just SDL.ScancodeHome
  "End"          -> Just SDL.ScancodeEnd
  "PageDown"     -> Just SDL.ScancodePageDown
  "PageUp"       -> Just SDL.ScancodePageUp
  "Apostrophe"   -> Just SDL.ScancodeApostrophe
  "Backslash"    -> Just SDL.ScancodeBackslash
  "Backspace"    -> Just SDL.ScancodeBackspace
  "CapsLock"     -> Just SDL.ScancodeCapsLock
  "Comma"        -> Just SDL.ScancodeComma
  "Equals"       -> Just SDL.ScancodeEquals
  "Escape"       -> Just SDL.ScancodeEscape
  "F1"           -> Just SDL.ScancodeF1
  "F2"           -> Just SDL.ScancodeF2
  "F3"           -> Just SDL.ScancodeF3
  "F4"           -> Just SDL.ScancodeF4
  "F5"           -> Just SDL.ScancodeF5
  "F6"           -> Just SDL.ScancodeF6
  "F7"           -> Just SDL.ScancodeF7
  "F8"           -> Just SDL.ScancodeF8
  "F9"           -> Just SDL.ScancodeF9
  "F10"          -> Just SDL.ScancodeF10
  "F11"          -> Just SDL.ScancodeF11
  "F12"          -> Just SDL.ScancodeF12
  "Grave"        -> Just SDL.ScancodeGrave
  "KP0"          -> Just SDL.ScancodeKP0
  "KP1"          -> Just SDL.ScancodeKP1
  "KP2"          -> Just SDL.ScancodeKP2
  "KP3"          -> Just SDL.ScancodeKP3
  "KP4"          -> Just SDL.ScancodeKP4
  "KP5"          -> Just SDL.ScancodeKP5
  "KP6"          -> Just SDL.ScancodeKP6
  "KP7"          -> Just SDL.ScancodeKP7
  "KP8"          -> Just SDL.ScancodeKP8
  "KP9"          -> Just SDL.ScancodeKP9
  "KPDivide"     -> Just SDL.ScancodeKPDivide
  "KPEnter"      -> Just SDL.ScancodeKPEnter
  "KPMinus"      -> Just SDL.ScancodeKPMinus
  "KPMultiply"   -> Just SDL.ScancodeKPMultiply
  "KPPeriod"     -> Just SDL.ScancodeKPPeriod
  "KPPlus"       -> Just SDL.ScancodeKPPlus
  "LAlt"         -> Just SDL.ScancodeLAlt
  "LCtrl"        -> Just SDL.ScancodeLCtrl
  "LeftBracket"  -> Just SDL.ScancodeLeftBracket
  "LShift"       -> Just SDL.ScancodeLShift
  "Minus"        -> Just SDL.ScancodeMinus
  "Pause"        -> Just SDL.ScancodePause
  "Period"       -> Just SDL.ScancodePeriod
  "RAlt"         -> Just SDL.ScancodeRAlt
  "RCtrl"        -> Just SDL.ScancodeRCtrl
  "Return"       -> Just SDL.ScancodeReturn
  "RightBracket" -> Just SDL.ScancodeRightBracket
  "RShift"       -> Just SDL.ScancodeRShift
  "ScrollLock"   -> Just SDL.ScancodeScrollLock
  "Semicolon"    -> Just SDL.ScancodeSemicolon
  "Slash"        -> Just SDL.ScancodeSlash
  "Space"        -> Just SDL.ScancodeSpace
  "Tab"          -> Just SDL.ScancodeTab
  _              -> Nothing
