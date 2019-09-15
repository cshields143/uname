module Namer ( cp2name ) where

import Hex
import Alias
import Hangul
import Ideograph
import Explicit
import Noname

cp2name :: Integer -> String
cp2name n = if (corrections n) /= "" then corrections n
  else if isHangul n then genHangulName n
  else if isIdeograph n then genIdeoName n
  else if isExplicit n then getExplicit n
  else if (ctrlFig n) /= "" then ctrlFig n
  else if isNoname n then getNoname n
  else if n < 1114112 then "<reserved>"
  else error "Not a valid codepoint"