module Hex ( hex2dec, dec2hex, unihex ) where

import Text.Printf

hexChar :: Char -> Integer
hexChar '0' = 0
hexChar '1' = 1
hexChar '2' = 2
hexChar '3' = 3
hexChar '4' = 4
hexChar '5' = 5
hexChar '6' = 6
hexChar '7' = 7
hexChar '8' = 8
hexChar '9' = 9
hexChar 'A' = 10
hexChar 'a' = 10
hexChar 'B' = 11
hexChar 'b' = 11
hexChar 'C' = 12
hexChar 'c' = 12
hexChar 'D' = 13
hexChar 'd' = 13
hexChar 'E' = 14
hexChar 'e' = 14
hexChar 'F' = 15
hexChar 'f' = 15
hexChar _ = error "Not hexadecimal"

hex2dec :: String -> Integer
hex2dec str = actual (reverse str)
  where actual []      = 0
        actual (x:xs)  = hexChar x + 16 * (actual xs)

decChar :: Integer -> Char
decChar 0  = '0'
decChar 1  = '1'
decChar 2  = '2'
decChar 3  = '3'
decChar 4  = '4'
decChar 5  = '5'
decChar 6  = '6'
decChar 7  = '7'
decChar 8  = '8'
decChar 9  = '9'
decChar 10 = 'A'
decChar 11 = 'B'
decChar 12 = 'C'
decChar 13 = 'D'
decChar 14 = 'E'
decChar 15 = 'F'
decChar _ = error "Not hexadecimalable"

dec2hex :: Integer -> String
dec2hex int = map decChar (reverse (listify int))
  where listify 0 = []
        listify x = (mod x 16) : listify (div x 16)

unihex :: Integer -> String
unihex n = printf "%04s" (dec2hex n)