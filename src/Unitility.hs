module Unitility ( unihex, inUniRange ) where

import Hex
import Text.Printf

unihex :: Integer -> String
unihex n = printf "%04s" (dec2hex n)

inRange :: Integer -> Integer -> Integer -> Bool
inRange x y z = x >= y && x <= z
inUniRange :: Integer -> String -> String -> Bool
inUniRange x a b = inRange x (hex2dec a) (hex2dec b)