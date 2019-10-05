module Explicit ( isExplicit, getExplicit ) where

import BasicMultiPlane
import SupplPlane

isExplicit :: Integer -> Bool
isExplicit n = (isBMP n) || (isSP n)

getExplicit :: Integer -> String
getExplicit n = if isBMP n then getBMP n
    else if isSP n then getSP n
