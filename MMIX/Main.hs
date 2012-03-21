-- Copyright 2012 Wu Xingbo <wuxb45@gmail.com>

module MMIX.Main (main) where

import MMIX.Run
import MMIX.Base

main :: IO ()
main = do
  mmix <- newDummyMMIX
  showMMIX mmix
  runMMIX mmix
  showMMIX mmix
  runMMIX mmix
  showMMIX mmix
