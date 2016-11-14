module Controls (controls) where

import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game

controls :: [(Command, [Key])]
controls =
  [ (CTranslate, [MouseButton LeftButton])
  , (CScale, [MouseButton RightButton])
  , (CBumpZoomOut, [MouseButton WheelUp, SpecialKey KeyPageUp])
  , (CBumpZoomIn, [MouseButton WheelDown, SpecialKey KeyPageDown])
  , (CBumpLeft, [SpecialKey KeyRight])
  , (CBumpRight, [SpecialKey KeyLeft])
  , (CBumpUp, [SpecialKey KeyDown])
  , (CBumpDown, [SpecialKey KeyUp]) ]