module Namer ( cp2name ) where

import Hex
import Alias
import Hangul
import Ideograph
import Explicit

cp2name :: Integer -> String
cp2name n = if (corrections n) /= "" then corrections n
  else if isHangul n then genHangulName n
  else if isIdeograph n then genIdeoName n
  else if isExplicit n then getExplicit n
  else if (ctrlFig n) /= "" then ctrlFig n
  else error "Not a valid code point"